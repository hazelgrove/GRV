module IndexMap = Map.Make (struct
  type t = Lang.Index.t

  let compare = compare
end)

type t = Ref of Vertex.t | Con of Vertex.t * t list IndexMap.t

let rec show : t -> string = function
  | Ref vertex -> "#" ^ Uuid.Id.show vertex.id
  | Con (vertex, children) ->
      Format.sprintf "Con (%s, {%s})" (Uuid.Id.show vertex.id)
        (String.concat "; "
           ( IndexMap.bindings children
           |> List.map (fun (index, ts) ->
                  Format.sprintf "%s -> [%s]"
                    (Lang.Index.short_name index)
                    (List.map show ts |> String.concat ", ")) ))

let rec mk ?(seen : Vertex.Set.t = Vertex.Set.empty) (source : Vertex.t)
    (descendants : Edge.Set.t) : t * Edge.Set.t =
  if Vertex.Set.mem source seen then (Ref source, descendants)
  else
    let seen : Vertex.Set.t = Vertex.Set.add source seen in
    let (children, others) : Edge.Set.t * Edge.Set.t =
      Edge.partition_set descendants source
    in
    let (map, others) : t list IndexMap.t * Edge.Set.t =
      Edge.Set.fold
        (fun e (map, others) ->
          assert (e.value.source.vertex = source);
          let (t, others') : t * Edge.Set.t = mk ~seen e.value.target others in
          ( IndexMap.update e.value.source.index
              (function None -> Some [ t ] | Some ts -> Some (t :: ts))
              map,
            Edge.Set.diff others others' ))
        children (IndexMap.empty, others)
    in
    (Con (source, map), others)

(* TODO: introduce "remaining" edge set to guarantee single pass *)
let rec reachable (live : Edge.Set.t) (seen : Vertex.Set.t) (vertex : Vertex.t)
    : t * Edge.Set.t =
  if Vertex.Set.mem vertex seen then (Ref vertex, Edge.Set.empty)
  else
    let children : Edge.Set.t =
      Edge.Set.filter (fun e -> e.value.source.vertex = vertex) live
    in
    let descendants : Edge.Set.t =
      Edge.Set.elements children
      |> List.map (fun (edge : Edge.t) -> edge.value.target)
      (* NOTE: SLOW *)
      |> List.map (reachable live (Vertex.Set.add vertex seen))
      |> List.split |> snd |> Edge.concat_sets
    in
    let edge_set = Edge.Set.union children descendants in
    let t = mk ~seen:(Vertex.Set.remove vertex seen) vertex edge_set |> fst in
    (t, edge_set)

type context = {
  live : Edge.Set.t;
  multiparent : Vertex.Set.t;
  deleted : Vertex.Set.t;
}

let context (graph : Graph.t) : context =
  {
    live = Graph.live_edges graph;
    multiparent = Graph.multiparented graph;
    deleted = Graph.deleted graph;
  }

let decompose ({ live; multiparent; deleted } : context) :
    t * t list * t list * t list =
  let (r, r_edges) : t * Edge.Set.t = reachable live multiparent Vertex.root in
  let (mp, mp_edges) : t list * Edge.Set.t =
    let ts, edge_sets =
      Vertex.Set.elements multiparent
      |> List.map (fun vertex ->
             reachable live (Vertex.Set.remove vertex multiparent) vertex)
      |> List.split
    in
    (ts, Edge.concat_sets edge_sets)
  in
  let (d, d_edges) : t list * Edge.Set.t =
    let ts, edge_sets =
      Vertex.Set.elements deleted
      |> List.map (reachable live multiparent)
      |> List.split
    in
    (ts, Edge.concat_sets edge_sets)
  in
  let rec simple_cycles (remaining : Edge.Set.t) (acc : (t * Edge.Set.t) list) :
      (t * Edge.Set.t) list =
    match Edge.Set.min_elt_opt remaining with
    | None -> acc
    | Some edge ->
        let (t, edge_set) : t * Edge.Set.t =
          reachable live multiparent edge.value.source.vertex
        in
        let remaining : Edge.Set.t = Edge.Set.diff remaining edge_set in
        simple_cycles remaining ((t, edge_set) :: acc)
  in
  let (sc, _sc_edges) : t list * Edge.Set.t =
    let ts, edge_sets =
      simple_cycles
        (Edge.Set.diff live (Edge.concat_sets [ r_edges; d_edges; mp_edges ]))
        []
      |> List.split
    in
    (ts, Edge.concat_sets edge_sets)
  in
  (r, d, mp, sc)

(*******************************************************************************
 * Unit Tests
 ******************************************************************************)

let%test "mk 0" =
  let t = mk Vertex.root Edge.Set.empty |> fst in
  let want = Con (Vertex.root, IndexMap.empty) in
  t = want

let%test "mk 1" =
  let target1 : Vertex.t = Vertex.mk Lang.Constructor.Exp_plus in
  let edge01 : Edge.t =
    Edge.mk (Cursor.mk Vertex.root Lang.Index.Root_root_root) target1
  in
  let t = mk Vertex.root (Edge.Set.singleton edge01) |> fst in
  let want =
    Con
      ( Vertex.root,
        IndexMap.singleton Lang.Index.Root_root_root
          [ Con (target1, IndexMap.empty) ] )
  in
  t = want

let%test "mk 2" =
  let target1 : Vertex.t = Vertex.mk Lang.Constructor.Exp_plus in
  let target2 : Vertex.t = Vertex.mk Lang.Constructor.Exp_times in
  let edge01 : Edge.t =
    Edge.mk (Cursor.mk Vertex.root Lang.Index.Root_root_root) target1
  in
  let edge02 : Edge.t =
    Edge.mk (Cursor.mk Vertex.root Lang.Index.Root_root_root) target2
  in
  let t = mk Vertex.root (Edge.Set.of_list [ edge01; edge02 ]) |> fst in
  let want =
    Con
      ( Vertex.root,
        IndexMap.singleton Lang.Index.Root_root_root
          [ Con (target2, IndexMap.empty); Con (target1, IndexMap.empty) ] )
  in
  t = want

let%test "mk 1.1" =
  let target1 : Vertex.t = Vertex.mk Lang.Constructor.Exp_plus in
  let target2 : Vertex.t = Vertex.mk Lang.Constructor.Exp_times in
  let edge01 : Edge.t =
    Edge.mk (Cursor.mk Vertex.root Lang.Index.Root_root_root) target1
  in
  let edge12 : Edge.t =
    Edge.mk (Cursor.mk target1 Lang.Index.Exp_plus_left) target2
  in
  let t = mk Vertex.root (Edge.Set.of_list [ edge01; edge12 ]) |> fst in
  let want =
    Con
      ( Vertex.root,
        IndexMap.singleton Lang.Index.Root_root_root
          [
            Con
              ( target1,
                IndexMap.singleton Lang.Index.Exp_plus_left
                  [ Con (target2, IndexMap.empty) ] );
          ] )
  in
  t = want

let%test "mk 1.2" =
  let target1 : Vertex.t = Vertex.mk Lang.Constructor.Exp_plus in
  let target2 : Vertex.t = Vertex.mk Lang.Constructor.Exp_times in
  let target3 : Vertex.t = Vertex.mk Lang.Constructor.Exp_app in
  let edge01 : Edge.t =
    Edge.mk (Cursor.mk Vertex.root Lang.Index.Root_root_root) target1
  in
  let edge12 : Edge.t =
    Edge.mk (Cursor.mk target1 Lang.Index.Exp_plus_left) target2
  in
  let edge13 : Edge.t =
    Edge.mk (Cursor.mk target1 Lang.Index.Exp_plus_right) target3
  in
  let t = mk Vertex.root (Edge.Set.of_list [ edge01; edge12; edge13 ]) |> fst in
  let want =
    Con
      ( Vertex.root,
        IndexMap.singleton Lang.Index.Root_root_root
          [
            Con
              ( target1,
                IndexMap.empty
                |> IndexMap.add Lang.Index.Exp_plus_left
                     [ Con (target2, IndexMap.empty) ]
                |> IndexMap.add Lang.Index.Exp_plus_right
                     [ Con (target3, IndexMap.empty) ] );
          ] )
  in
  t = want

let%test "mk 1.1.1" =
  let target1 : Vertex.t = Vertex.mk Lang.Constructor.Exp_plus in
  let target2 : Vertex.t = Vertex.mk Lang.Constructor.Exp_times in
  let target3 : Vertex.t = Vertex.mk Lang.Constructor.Exp_app in
  let edge01 : Edge.t =
    Edge.mk (Cursor.mk Vertex.root Lang.Index.Root_root_root) target1
  in
  let edge12 : Edge.t =
    Edge.mk (Cursor.mk target1 Lang.Index.Exp_plus_left) target2
  in
  let edge23 : Edge.t =
    Edge.mk (Cursor.mk target2 Lang.Index.Exp_plus_right) target3
  in
  let t = mk Vertex.root (Edge.Set.of_list [ edge01; edge12; edge23 ]) |> fst in
  let want =
    Con
      ( Vertex.root,
        IndexMap.singleton Lang.Index.Root_root_root
          [
            Con
              ( target1,
                IndexMap.singleton Lang.Index.Exp_plus_left
                  [
                    Con
                      ( target2,
                        IndexMap.singleton Lang.Index.Exp_plus_right
                          [ Con (target3, IndexMap.empty) ] );
                  ] );
          ] )
  in
  t = want

let%test "mk sc" =
  let target1 : Vertex.t = Vertex.mk Lang.Constructor.Exp_plus in
  let edge01 : Edge.t =
    Edge.mk (Cursor.mk Vertex.root Lang.Index.Root_root_root) target1
  in
  let edge10 : Edge.t =
    Edge.mk (Cursor.mk target1 Lang.Index.Exp_plus_left) Vertex.root
  in
  let t = mk Vertex.root (Edge.Set.of_list [ edge01; edge10 ]) |> fst in
  let want =
    Con
      ( Vertex.root,
        IndexMap.singleton Lang.Index.Root_root_root
          [
            Con
              ( target1,
                IndexMap.empty
                |> IndexMap.add Lang.Index.Exp_plus_left [ Ref Vertex.root ] );
          ] )
  in
  t = want

let%test "mk mp" =
  let target1 : Vertex.t = Vertex.mk Lang.Constructor.Exp_plus in
  let edge01 : Edge.t =
    Edge.mk (Cursor.mk Vertex.root Lang.Index.Root_root_root) target1
  in
  let edge10 : Edge.t =
    Edge.mk (Cursor.mk target1 Lang.Index.Exp_plus_left) Vertex.root
  in
  let edge10' : Edge.t =
    Edge.mk (Cursor.mk target1 Lang.Index.Exp_plus_right) Vertex.root
  in
  let t =
    mk Vertex.root (Edge.Set.of_list [ edge01; edge10; edge10' ]) |> fst
  in
  let want =
    Con
      ( Vertex.root,
        IndexMap.singleton Lang.Index.Root_root_root
          [
            Con
              ( target1,
                IndexMap.empty
                |> IndexMap.add Lang.Index.Exp_plus_left [ Ref Vertex.root ]
                |> IndexMap.add Lang.Index.Exp_plus_right [ Ref Vertex.root ] );
          ] )
  in
  t = want

let%test "decompose 0" =
  let graph = Graph.empty in
  let ctx = context graph in
  decompose ctx = (Con (Vertex.root, IndexMap.empty), [], [], [])

let%test "decompose 0->1" =
  let vertex1 : Vertex.t = Vertex.mk Lang.Constructor.Exp_plus in
  let edge01 : Edge.t =
    Edge.mk (Cursor.mk Vertex.root Lang.Index.Root_root_root) vertex1
  in
  let graph : Graph.t = Edge.Map.(empty |> add edge01 Edge_state.Created) in
  let ctx = context graph in
  decompose ctx
  = ( Con
        ( Vertex.root,
          IndexMap.(
            empty
            |> add Lang.Index.Root_root_root [ Con (vertex1, IndexMap.empty) ])
        ),
      [],
      [],
      [] )

let%test "decompose 0 1" =
  let vertex1 : Vertex.t = Vertex.mk Lang.Constructor.Exp_plus in
  let edge01 : Edge.t =
    Edge.mk (Cursor.mk Vertex.root Lang.Index.Root_root_root) vertex1
  in
  let graph : Graph.t = Edge.Map.(empty |> add edge01 Edge_state.Destroyed) in
  let ctx = context graph in
  decompose ctx
  = ( Con (Vertex.root, IndexMap.empty),
      [ Con (vertex1, IndexMap.empty) ],
      [],
      [] )

let%test "decompose 0->2 1" =
  let vertex1 : Vertex.t = Vertex.mk Lang.Constructor.Exp_plus in
  let vertex2 : Vertex.t = Vertex.mk Lang.Constructor.Exp_times in
  let edge01 : Edge.t =
    Edge.mk (Cursor.mk Vertex.root Lang.Index.Root_root_root) vertex1
  in
  let edge02 : Edge.t =
    Edge.mk (Cursor.mk Vertex.root Lang.Index.Root_root_root) vertex2
  in
  let graph : Graph.t =
    Edge.Map.empty
    |> Edge.Map.add edge01 Edge_state.Destroyed
    |> Edge.Map.add edge02 Edge_state.Created
  in
  let ctx = context graph in
  decompose ctx
  = ( Con
        ( Vertex.root,
          IndexMap.singleton Lang.Index.Root_root_root
            [ Con (vertex2, IndexMap.empty) ] ),
      [ Con (vertex1, IndexMap.empty) ],
      [],
      [] )

let%test "decompose 0->2->3 1" =
  let vertex1 : Vertex.t = Vertex.mk Lang.Constructor.Exp_plus in
  let vertex2 : Vertex.t = Vertex.mk Lang.Constructor.Exp_times in
  let vertex3 : Vertex.t = Vertex.mk Lang.Constructor.Exp_app in
  let edge01 : Edge.t =
    Edge.mk (Cursor.mk Vertex.root Lang.Index.Root_root_root) vertex1
  in
  let edge02 : Edge.t =
    Edge.mk (Cursor.mk Vertex.root Lang.Index.Root_root_root) vertex2
  in
  let edge23 : Edge.t =
    Edge.mk (Cursor.mk vertex2 Lang.Index.Exp_times_left) vertex3
  in
  let graph : Graph.t =
    Edge.Map.empty
    |> Edge.Map.add edge01 Edge_state.Destroyed
    |> Edge.Map.add edge02 Edge_state.Created
    |> Edge.Map.add edge23 Edge_state.Created
  in
  let ctx = context graph in
  decompose ctx
  = ( Con
        ( Vertex.root,
          IndexMap.singleton Lang.Index.Root_root_root
            [
              Con
                ( vertex2,
                  IndexMap.singleton Lang.Index.Exp_times_left
                    [ Con (vertex3, IndexMap.empty) ] );
            ] ),
      [ Con (vertex1, IndexMap.empty) ],
      [],
      [] )

let%test "decompose 0->2->3 1->3" =
  let vertex1 : Vertex.t = Vertex.mk Lang.Constructor.Exp_plus in
  let vertex2 : Vertex.t = Vertex.mk Lang.Constructor.Exp_times in
  let vertex3 : Vertex.t = Vertex.mk Lang.Constructor.Exp_app in
  let edge01 : Edge.t =
    Edge.mk (Cursor.mk Vertex.root Lang.Index.Root_root_root) vertex1
  in
  let edge02 : Edge.t =
    Edge.mk (Cursor.mk Vertex.root Lang.Index.Root_root_root) vertex2
  in
  let edge23 : Edge.t =
    Edge.mk (Cursor.mk vertex2 Lang.Index.Exp_times_left) vertex3
  in
  let edge13 : Edge.t =
    Edge.mk (Cursor.mk vertex1 Lang.Index.Exp_plus_left) vertex3
  in
  let graph : Graph.t =
    Edge.Map.empty
    |> Edge.Map.add edge01 Edge_state.Destroyed
    |> Edge.Map.add edge02 Edge_state.Created
    |> Edge.Map.add edge23 Edge_state.Created
    |> Edge.Map.add edge13 Edge_state.Created
  in
  let ctx = context graph in
  decompose ctx
  = ( Con
        ( Vertex.root,
          IndexMap.singleton Lang.Index.Root_root_root
            [
              Con
                ( vertex2,
                  IndexMap.singleton Lang.Index.Exp_times_left [ Ref vertex3 ]
                );
            ] ),
      [ Con (vertex1, IndexMap.singleton Exp_plus_left [ Ref vertex3 ]) ],
      [ Con (vertex3, IndexMap.empty) ],
      [] )

let%test "decompose 0->2<->3<-1" =
  let v1 : Vertex.t = Vertex.mk Lang.Constructor.Exp_plus in
  let v2 : Vertex.t = Vertex.mk Lang.Constructor.Exp_times in
  let v3 : Vertex.t = Vertex.mk Lang.Constructor.Exp_app in
  let e01 : Edge.t =
    Edge.mk (Cursor.mk Vertex.root Lang.Index.Root_root_root) v1
  in
  let e02 : Edge.t =
    Edge.mk (Cursor.mk Vertex.root Lang.Index.Root_root_root) v2
  in
  let e23 : Edge.t = Edge.mk (Cursor.mk v2 Lang.Index.Exp_times_left) v3 in
  let e13 : Edge.t = Edge.mk (Cursor.mk v1 Lang.Index.Exp_plus_left) v3 in
  let e32 : Edge.t = Edge.mk (Cursor.mk v3 Lang.Index.Exp_app_arg) v2 in
  let graph : Graph.t =
    Edge.Map.empty
    |> Edge.Map.add e01 Edge_state.Destroyed
    |> Edge.Map.add e02 Edge_state.Created
    |> Edge.Map.add e23 Edge_state.Created
    |> Edge.Map.add e13 Edge_state.Created
    |> Edge.Map.add e32 Edge_state.Created
  in
  let ctx = context graph in
  let got_r, got_d, got_mp, got_sc = decompose ctx in
  let want_r, want_d, want_mp, want_sc =
    ( Con (Vertex.root, IndexMap.singleton Lang.Index.Root_root_root [ Ref v2 ]),
      [ Con (v1, IndexMap.singleton Lang.Index.Exp_plus_left [ Ref v3 ]) ],
      [
        Con (v2, IndexMap.singleton Lang.Index.Exp_times_left [ Ref v3 ]);
        Con (v3, IndexMap.singleton Lang.Index.Exp_app_arg [ Ref v2 ]);
      ],
      [] )
  in
  (got_r, got_d, got_mp, got_sc) = (want_r, want_d, want_mp, want_sc)

let%test "decompose 0 2<->3<-1" =
  let v1 : Vertex.t = Vertex.mk Lang.Constructor.Exp_plus in
  let v2 : Vertex.t = Vertex.mk Lang.Constructor.Exp_times in
  let v3 : Vertex.t = Vertex.mk Lang.Constructor.Exp_app in
  let e01 : Edge.t =
    Edge.mk (Cursor.mk Vertex.root Lang.Index.Root_root_root) v1
  in
  let e02 : Edge.t =
    Edge.mk (Cursor.mk Vertex.root Lang.Index.Root_root_root) v2
  in
  let e23 : Edge.t = Edge.mk (Cursor.mk v2 Lang.Index.Exp_times_left) v3 in
  let e13 : Edge.t = Edge.mk (Cursor.mk v1 Lang.Index.Exp_plus_left) v3 in
  let e32 : Edge.t = Edge.mk (Cursor.mk v3 Lang.Index.Exp_app_arg) v2 in
  let graph : Graph.t =
    Edge.Map.empty
    |> Edge.Map.add e01 Edge_state.Destroyed
    |> Edge.Map.add e02 Edge_state.Destroyed
    |> Edge.Map.add e23 Edge_state.Created
    |> Edge.Map.add e13 Edge_state.Created
    |> Edge.Map.add e32 Edge_state.Created
  in
  let ctx = context graph in
  let got_r, got_d, got_mp, got_sc = decompose ctx in
  let want_r, want_d, want_mp, want_sc =
    ( Con (Vertex.root, IndexMap.empty),
      [ Con (v1, IndexMap.singleton Lang.Index.Exp_plus_left [ Ref v3 ]) ],
      [
        Con
          ( v3,
            IndexMap.singleton Lang.Index.Exp_app_arg
              [
                Con (v2, IndexMap.singleton Lang.Index.Exp_times_left [ Ref v3 ]);
              ] );
      ],
      [] )
  in
  (got_r, got_d, got_mp, got_sc) = (want_r, want_d, want_mp, want_sc)

let%test "decompose 0 2<->3 1" =
  let v1 : Vertex.t = Vertex.mk Lang.Constructor.Exp_plus in
  let v2 : Vertex.t = Vertex.mk Lang.Constructor.Exp_times in
  let v3 : Vertex.t = Vertex.mk Lang.Constructor.Exp_app in
  let e01 : Edge.t =
    Edge.mk (Cursor.mk Vertex.root Lang.Index.Root_root_root) v1
  in
  let e02 : Edge.t =
    Edge.mk (Cursor.mk Vertex.root Lang.Index.Root_root_root) v2
  in
  let e23 : Edge.t = Edge.mk (Cursor.mk v2 Lang.Index.Exp_times_left) v3 in
  let e13 : Edge.t = Edge.mk (Cursor.mk v1 Lang.Index.Exp_plus_left) v3 in
  let e32 : Edge.t = Edge.mk (Cursor.mk v3 Lang.Index.Exp_app_arg) v2 in
  let graph : Graph.t =
    Edge.Map.empty
    |> Edge.Map.add e01 Edge_state.Destroyed
    |> Edge.Map.add e02 Edge_state.Destroyed
    |> Edge.Map.add e23 Edge_state.Created
    |> Edge.Map.add e13 Edge_state.Destroyed
    |> Edge.Map.add e32 Edge_state.Created
  in
  let ctx = context graph in
  let got_r, got_d, got_mp, got_sc = decompose ctx in
  let want_r, want_d, want_mp, want_sc =
    ( Con (Vertex.root, IndexMap.empty),
      [ Con (v1, IndexMap.empty) ],
      [],
      [
        Con
          ( v2,
            IndexMap.singleton Lang.Index.Exp_times_left
              [ Con (v3, IndexMap.singleton Lang.Index.Exp_app_arg [ Ref v2 ]) ]
          );
      ] )
  in
  (* Format.printf "\n--\n\n GOT  R: %s\n%!" (show got_r);
     Format.printf "WANT  R: %s\n%!" (show want_r);
     Format.printf " GOT  D: [%s]\n%!" (String.concat "; " (List.map show got_d));
     Format.printf "WANT  D: [%s]\n%!" (String.concat "; " (List.map show want_d));
     Format.printf " GOT MP: [%s]\n%!" (String.concat "; " (List.map show got_mp));
     Format.printf "WANT MP: [%s]\n%!" (String.concat "; " (List.map show want_mp));
     Format.printf " GOT SC: [%s]\n%!" (String.concat "; " (List.map show got_sc));
     Format.printf "WANT SC: [%s]\n%!" (String.concat "; " (List.map show want_sc)); *)
  (got_r, got_d, got_mp, got_sc) = (want_r, want_d, want_mp, want_sc)
