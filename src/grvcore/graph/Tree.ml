module IndexMap = Map.Make (struct
  type t = Lang.Index.t

  let compare = compare
end)

type t = Ref of Vertex.t | Con of Vertex.t * t list IndexMap.t

(* let rec to_string : t -> string = function
  | Ref vertex -> "#" ^ Uuid.Id.to_string vertex.id
  | Con (vertex, children) ->
      IndexMap.bindings children
      |> List.map (fun (index, ts) ->
             List.map to_string ts |> String.concat ", "
             |> Format.sprintf "%s -> [%s]" (Lang.Index.short_name index))
      |> String.concat "; "
      |> Format.sprintf "Con(%s, {%s})" (Uuid.Id.to_string vertex.id) *)

let rec mk ?(seen : Vertex.Set.t = Vertex.Set.empty) (source : Vertex.t)
    (descendants : Edge.Set.t) : t * Edge.Set.t =
  if Vertex.Set.mem source seen then (Ref source, descendants)
  else
    let seen = Vertex.Set.add source seen in
    let these, others = Edge.partition_set descendants source in
    let children, others =
      (IndexMap.empty, others)
      |> Edge.Set.fold
           (fun edge (children, others) ->
             assert (edge.value.source.vertex = source);
             let tree, others' = mk ~seen edge.value.target others in
             ( IndexMap.update edge.value.source.index
                 (function
                   | None -> Some [ tree ] | Some trees -> Some (tree :: trees))
                 children,
               Edge.Set.diff others others' ))
           these
    in
    (Con (source, children), others)

let rec reachable (unseen : Edge.Set.t) (seen : Vertex.Set.t)
    (vertex : Vertex.t) : t * Edge.Set.t =
  if Vertex.Set.mem vertex seen then (Ref vertex, Edge.Set.empty)
  else
    let children = Edge.children_in_set unseen vertex in
    let descendants : Edge.Set.t =
      Edge.Set.elements children
      |> List.map (fun (edge : Edge.t) ->
             (* NOTE: is this slow? *)
             edge.value.target
             |> reachable unseen (Vertex.Set.add vertex seen)
             |> snd)
      |> Edge.concat_sets
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

let consolidate (results : (t * Edge.Set.t) list) : t list * Edge.Set.t =
  let trees, edge_sets = List.split results in
  (trees, Edge.concat_sets edge_sets)

let decompose ({ live; multiparent; deleted } : context) :
    t * t list * t list * t list =
  let r, r_edges = reachable live multiparent Vertex.root in
  let mp, mp_edges =
    Vertex.Set.elements multiparent
    |> List.map (fun v -> reachable live (Vertex.Set.remove v multiparent) v)
    |> consolidate
  in
  let (d, d_edges) : t list * Edge.Set.t =
    Vertex.Set.elements deleted
    |> List.map (reachable live multiparent)
    |> consolidate
  in
  let rec simple_cycles (remaining : Edge.Set.t) (acc : (t * Edge.Set.t) list) :
      (t * Edge.Set.t) list =
    Edge.Set.min_elt_opt remaining
    |> Option.fold ~none:acc ~some:(fun (edge : Edge.t) ->
           let result = reachable live multiparent edge.value.source.vertex in
           let remaining = snd result |> Edge.Set.diff remaining in
           simple_cycles remaining (result :: acc))
  in
  let (sc, _sc_edges) : t list * Edge.Set.t =
    let seen = Edge.concat_sets [ r_edges; d_edges; mp_edges ] in
    simple_cycles (Edge.Set.diff live seen) [] |> consolidate
  in
  (r, d, mp, sc)

(*******************************************************************************
 * Unit Tests
 ******************************************************************************)

(* TODO: drop spurions "Lang.Constructr." *)
let%test "mk 0" =
  let got = mk Vertex.root Edge.Set.empty |> fst in
  let want = Con (Vertex.root, IndexMap.empty) in
  got = want

let%test "mk 1" =
  let v1 = Vertex.mk Exp_plus in
  let e01 = Edge.mk Cursor.root v1 in
  let got = mk Vertex.root (Edge.Set.of_list [ e01 ]) |> fst in
  let want =
    Con
      ( Vertex.root,
        IndexMap.singleton Root_root_root [ Con (v1, IndexMap.empty) ] )
  in
  got = want

let%test "mk 2" =
  let v1 = Vertex.mk Exp_plus in
  let v2 = Vertex.mk Exp_times in
  let e01 = Edge.mk Cursor.root v1 in
  let e02 = Edge.mk Cursor.root v2 in
  let got = mk Vertex.root (Edge.Set.of_list [ e01; e02 ]) |> fst in
  let want =
    Con
      ( Vertex.root,
        IndexMap.singleton Root_root_root
          [ Con (v2, IndexMap.empty); Con (v1, IndexMap.empty) ] )
  in
  got = want

let%test "mk 1.1" =
  let v1 = Vertex.mk Exp_plus in
  let v2 = Vertex.mk Exp_times in
  let e01 = Edge.mk Cursor.root v1 in
  let e02 = Edge.mk Cursor.{ vertex = v1; index = Exp_plus_left } v2 in
  let got = mk Vertex.root (Edge.Set.of_list [ e01; e02 ]) |> fst in
  let want =
    Con
      ( Vertex.root,
        IndexMap.singleton Root_root_root
          [
            Con
              (v1, IndexMap.singleton Exp_plus_left [ Con (v2, IndexMap.empty) ]);
          ] )
  in
  got = want

let%test "mk 1.2" =
  let v1 = Vertex.mk Exp_plus in
  let v2 = Vertex.mk Exp_times in
  let v3 = Vertex.mk Exp_app in
  let e01 = Edge.mk Cursor.root v1 in
  let e12 = Edge.mk Cursor.{ vertex = v1; index = Exp_plus_left } v2 in
  let e13 = Edge.mk Cursor.{ vertex = v1; index = Exp_plus_right } v3 in
  let got = mk Vertex.root (Edge.Set.of_list [ e01; e12; e13 ]) |> fst in
  let want =
    Con
      ( Vertex.root,
        IndexMap.singleton Root_root_root
          [
            Con
              ( v1,
                IndexMap.empty
                |> IndexMap.add Exp_plus_left [ Con (v2, IndexMap.empty) ]
                |> IndexMap.add Exp_plus_right [ Con (v3, IndexMap.empty) ] );
          ] )
  in
  got = want

let%test "mk 1.1.1" =
  let v1 = Vertex.mk Exp_plus in
  let v2 = Vertex.mk Exp_times in
  let v3 = Vertex.mk Exp_app in
  let e01 = Edge.mk Cursor.root v1 in
  let e12 = Edge.mk Cursor.{ vertex = v1; index = Exp_plus_left } v2 in
  let e23 = Edge.mk Cursor.{ vertex = v2; index = Exp_plus_right } v3 in
  let got = mk Vertex.root (Edge.Set.of_list [ e01; e12; e23 ]) |> fst in
  let want =
    Con
      ( Vertex.root,
        IndexMap.(
          singleton Root_root_root
            [
              Con
                ( v1,
                  singleton Exp_plus_left
                    [ Con (v2, singleton Exp_plus_right [ Con (v3, empty) ]) ]
                );
            ]) )
  in
  got = want

let%test "mk sc" =
  let v1 = Vertex.mk Exp_plus in
  let e01 = Edge.mk Cursor.root v1 in
  let e10 = Edge.mk Cursor.{ vertex = v1; index = Exp_plus_left } Vertex.root in
  let got = mk Vertex.root (Edge.Set.of_list [ e01; e10 ]) |> fst in
  let want =
    Con
      ( Vertex.root,
        IndexMap.(
          singleton Root_root_root
            [ Con (v1, empty |> add Exp_plus_left [ Ref Vertex.root ]) ]) )
  in
  got = want

let%test "mk mp" =
  let v1 = Vertex.mk Exp_plus in
  let e01 = Edge.mk Cursor.root v1 in
  let e10 = Edge.mk Cursor.{ vertex = v1; index = Exp_plus_left } Vertex.root in
  let e10' =
    Edge.mk Cursor.{ vertex = v1; index = Exp_plus_right } Vertex.root
  in
  let got = mk Vertex.root (Edge.Set.of_list [ e01; e10; e10' ]) |> fst in
  let want =
    Con
      ( Vertex.root,
        IndexMap.(
          singleton Root_root_root
            [
              Con
                ( v1,
                  empty
                  |> add Exp_plus_left [ Ref Vertex.root ]
                  |> add Exp_plus_right [ Ref Vertex.root ] );
            ]) )
  in
  got = want

let%test "decompose 0" =
  let got = Graph.empty |> context |> decompose in
  let want = (Con (Vertex.root, IndexMap.empty), [], [], []) in
  got = want

let%test "decompose 0->1" =
  let v1 = Vertex.mk Exp_plus in
  let e01 = Edge.mk Cursor.root v1 in
  let got =
    Edge.Map.(empty |> add e01 Edge_state.Created |> context |> decompose)
  in
  let want =
    ( Con
        (Vertex.root, IndexMap.(empty |> add Root_root_root [ Con (v1, empty) ])),
      [],
      [],
      [] )
  in
  got = want

let%test "decompose 0 1" =
  let v1 = Vertex.mk Exp_plus in
  let e01 = Edge.mk Cursor.root v1 in
  let got =
    Edge.Map.(empty |> add e01 Edge_state.Destroyed |> context |> decompose)
  in
  let want =
    (Con (Vertex.root, IndexMap.empty), [ Con (v1, IndexMap.empty) ], [], [])
  in
  got = want

let%test "decompose 0->2 1" =
  let v1 = Vertex.mk Exp_plus in
  let v2 = Vertex.mk Exp_times in
  let e01 = Edge.mk Cursor.root v1 in
  let e02 = Edge.mk Cursor.root v2 in
  let got =
    Edge.Map.(
      empty
      |> add e01 Edge_state.Destroyed
      |> add e02 Edge_state.Created |> context |> decompose)
  in
  let want =
    ( Con (Vertex.root, IndexMap.(singleton Root_root_root [ Con (v2, empty) ])),
      [ Con (v1, IndexMap.empty) ],
      [],
      [] )
  in
  got = want

let%test "decompose 0->2->3 1" =
  let v1 = Vertex.mk Exp_plus in
  let v2 = Vertex.mk Exp_times in
  let v3 = Vertex.mk Exp_app in
  let e01 = Edge.mk Cursor.root v1 in
  let e02 = Edge.mk Cursor.root v2 in
  let e23 = Edge.mk Cursor.{ vertex = v2; index = Exp_times_left } v3 in
  let got =
    Edge.Map.(
      empty
      |> add e01 Edge_state.Destroyed
      |> add e02 Edge_state.Created |> add e23 Edge_state.Created |> context
      |> decompose)
  in
  let want =
    ( Con
        ( Vertex.root,
          IndexMap.(
            singleton Root_root_root
              [ Con (v2, singleton Exp_times_left [ Con (v3, empty) ]) ]) ),
      [ Con (v1, IndexMap.empty) ],
      [],
      [] )
  in
  got = want

let%test "decompose 0->2->3 1->3" =
  let v1 = Vertex.mk Exp_plus in
  let v2 = Vertex.mk Exp_times in
  let v3 = Vertex.mk Exp_app in
  let e01 = Edge.mk Cursor.root v1 in
  let e02 = Edge.mk Cursor.root v2 in
  let e23 = Edge.mk Cursor.{ vertex = v2; index = Exp_times_left } v3 in
  let e13 = Edge.mk Cursor.{ vertex = v1; index = Exp_plus_left } v3 in
  let got =
    Edge.Map.(
      empty
      |> add e01 Edge_state.Destroyed
      |> add e02 Edge_state.Created |> add e23 Edge_state.Created
      |> add e13 Edge_state.Created |> context |> decompose)
  in
  let want =
    ( Con
        ( Vertex.root,
          IndexMap.(
            singleton Root_root_root
              [ Con (v2, singleton Exp_times_left [ Ref v3 ]) ]) ),
      [ Con (v1, IndexMap.singleton Exp_plus_left [ Ref v3 ]) ],
      [ Con (v3, IndexMap.empty) ],
      [] )
  in
  got = want

let%test "decompose 0->2<->3<-1" =
  let v1 = Vertex.mk Exp_plus in
  let v2 = Vertex.mk Exp_times in
  let v3 = Vertex.mk Exp_app in
  let e01 = Edge.mk Cursor.root v1 in
  let e02 = Edge.mk Cursor.root v2 in
  let e23 = Edge.mk Cursor.{ vertex = v2; index = Exp_times_left } v3 in
  let e13 = Edge.mk Cursor.{ vertex = v1; index = Exp_plus_left } v3 in
  let e32 = Edge.mk Cursor.{ vertex = v3; index = Exp_app_arg } v2 in
  let got =
    Edge.Map.(
      empty
      |> add e01 Edge_state.Destroyed
      |> add e02 Edge_state.Created |> add e23 Edge_state.Created
      |> add e13 Edge_state.Created |> add e32 Edge_state.Created |> context
      |> decompose)
  in
  let want =
    ( Con (Vertex.root, IndexMap.singleton Root_root_root [ Ref v2 ]),
      [ Con (v1, IndexMap.singleton Exp_plus_left [ Ref v3 ]) ],
      [
        Con (v2, IndexMap.singleton Exp_times_left [ Ref v3 ]);
        Con (v3, IndexMap.singleton Exp_app_arg [ Ref v2 ]);
      ],
      [] )
  in
  got = want

let%test "decompose 0 2<->3<-1" =
  let v1 = Vertex.mk Exp_plus in
  let v2 = Vertex.mk Exp_times in
  let v3 = Vertex.mk Exp_app in
  let e01 = Edge.mk Cursor.root v1 in
  let e02 = Edge.mk Cursor.root v2 in
  let e23 = Edge.mk Cursor.{ vertex = v2; index = Exp_times_left } v3 in
  let e13 = Edge.mk Cursor.{ vertex = v1; index = Exp_plus_left } v3 in
  let e32 = Edge.mk Cursor.{ vertex = v3; index = Exp_app_arg } v2 in
  let got =
    Edge.Map.(
      empty
      |> add e01 Edge_state.Destroyed
      |> add e02 Edge_state.Destroyed
      |> add e23 Edge_state.Created |> add e13 Edge_state.Created
      |> add e32 Edge_state.Created |> context |> decompose)
  in
  let want =
    ( Con (Vertex.root, IndexMap.empty),
      [ Con (v1, IndexMap.singleton Exp_plus_left [ Ref v3 ]) ],
      [
        Con
          ( v3,
            IndexMap.(
              singleton Exp_app_arg
                [ Con (v2, singleton Exp_times_left [ Ref v3 ]) ]) );
      ],
      [] )
  in
  got = want

let%test "decompose 0 2<->3 1" =
  let v1 = Vertex.mk Exp_plus in
  let v2 = Vertex.mk Exp_times in
  let v3 = Vertex.mk Exp_app in
  let e01 = Edge.mk Cursor.root v1 in
  let e02 = Edge.mk Cursor.root v2 in
  let e23 = Edge.mk Cursor.{ vertex = v2; index = Exp_times_left } v3 in
  let e13 = Edge.mk Cursor.{ vertex = v1; index = Exp_plus_left } v3 in
  let e32 = Edge.mk Cursor.{ vertex = v3; index = Exp_app_arg } v2 in
  let got =
    Edge.Map.(
      empty
      |> add e01 Edge_state.Destroyed
      |> add e02 Edge_state.Destroyed
      |> add e23 Edge_state.Created
      |> add e13 Edge_state.Destroyed
      |> add e32 Edge_state.Created |> context |> decompose)
  in
  let want =
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
  got = want
