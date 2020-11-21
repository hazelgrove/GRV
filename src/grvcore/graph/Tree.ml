module IndexMap = Map.Make (struct
  type t = Lang.Index.t

  let compare = compare
end)

type t = Ref of Vertex.t | Con of Vertex.t * t list IndexMap.t

let rec show : t -> string = function
  | Ref vertex -> "#" ^ Uuid.Id.show vertex.id
  | Con (vertex, children) ->
      "Con (" ^ Uuid.Id.show vertex.id ^ ", {"
      ^ String.concat "; "
          (IndexMap.fold
             (fun index ts strs ->
               let str =
                 Lang.Index.short_name index
                 ^ " -> " ^ "["
                 ^ String.concat ", " (List.map show ts)
                 ^ "]"
               in
               str :: strs)
             children [])
      ^ "})"

let rec vertexes : t -> Vertex.Set.t = function
  | Ref vertex -> Vertex.Set.singleton vertex
  | Con (vertex, map) ->
      IndexMap.bindings map |> List.map snd
      |> List.map (List.map vertexes)
      |> List.map (List.map Vertex.Set.elements)
      |> List.map List.concat |> List.concat |> Vertex.Set.of_list
      |> Vertex.Set.add vertex

let partition (edges : Edge.Set.t) (pivot : Vertex.t) : Edge.Set.t * Edge.Set.t
    =
  Edge.Set.partition (fun (e : Edge.t) -> e.value.source.vertex = pivot) edges

let rec mk ?(seen : Vertex.Set.t = Vertex.Set.empty) (source : Vertex.t)
    (descendants : Edge.Set.t) : t * Edge.Set.t =
  if Vertex.Set.mem source seen then (Ref source, descendants)
  else
    let seen = Vertex.Set.add source seen in
    let (children, others) : Edge.Set.t * Edge.Set.t =
      partition descendants source
    in
    if Edge.Set.is_empty children then (Con (source, IndexMap.empty), others)
    else
      let (map, others) : t list IndexMap.t * Edge.Set.t =
        Edge.Set.fold
          (fun e (map, others) ->
            assert (e.value.source.vertex = source);
            let (t, others') : t * Edge.Set.t =
              mk ~seen e.value.target others
            in
            ( IndexMap.update e.value.source.index
                (function None -> Some [ t ] | Some ts -> Some (t :: ts))
                map,
              Edge.Set.diff others others' ))
          children (IndexMap.empty, others)
      in
      (Con (source, map), others)

let edge_set_concat (sets : Edge.Set.t list) : Edge.Set.t =
  List.map Edge.Set.elements sets |> List.concat |> Edge.Set.of_list

(* TODO: introduce "remaining" edge set to guarantee single pass *)
let rec reachable (live : Edge.Set.t) (seen : Vertex.Set.t) (vertex : Vertex.t)
    : t * Edge.Set.t =
  (* Format.printf "REACHABLE %s%!" (Vertex.show vertex); *)
  if Vertex.Set.mem vertex seen then
    ((* Format.printf "REF%!"; *)
     Ref vertex, Edge.Set.empty)
  else
    (* Format.printf "CON%!"; *)
    let children : Edge.Set.t =
      Edge.Set.filter (fun e -> e.value.source.vertex = vertex) live
    in

    (* Format.printf "CHILDREN = ";
       Edge.print_set children;
       Format.printf "%!"; *)
    let descendants : Edge.Set.t =
      Edge.Set.elements children
      |> List.map (fun (edge : Edge.t) -> edge.value.target)
      (* NOTE: SLOW *)
      |> List.map (reachable live (Vertex.Set.add vertex seen))
      |> List.split |> snd |> edge_set_concat
    in

    (* Format.printf "DESCENDANTS = ";
       Edge.print_set descendants;
       Format.printf "%!"; *)
    let edge_set = Edge.Set.union children descendants in

    (* Format.printf "EDGE_SET = ";
       Edge.print_set edge_set;
       Format.printf "%!"; *)
    let t = mk ~seen:(Vertex.Set.remove vertex seen) vertex edge_set |> fst in

    (* Format.printf "GOT %s%!" (show t); *)
    (t, edge_set)

let decompose (graph : Graph.t) : t * t list * t list * t list =
  Format.printf "\nBEGIN DECOMP%!";

  let all : Edge.Set.t =
    Edge.Map.bindings graph |> List.map fst |> Edge.Set.of_list
  in

  Format.printf "all = ";
  Edge.print_set all;
  Format.printf "%!";

  let live : Edge.Set.t =
    graph
    |> Edge.Map.filter (fun _ ->
         function Edge_state.Created -> true | _ -> false)
    |> Edge.Map.bindings |> List.map fst |> Edge.Set.of_list
  in

  Format.printf "live = ";
  Edge.print_set live;
  Format.printf "%!";

  let multiparent : Vertex.Set.t =
    (* find them all in one pass *)
    let hist : int Vertex.Map.t =
      Edge.Set.fold
        (fun edge hist ->
          hist
          |> Vertex.Map.update edge.value.target (function
               | None -> Some 1
               | Some _ -> Some 2))
        live Vertex.Map.empty
    in
    Vertex.Map.filter (fun _ count -> count = 2) hist
    |> Vertex.Map.bindings |> List.map fst |> Vertex.Set.of_list
  in

  Format.printf "multiparent = ";
  Vertex.print_set multiparent;
  Format.printf "%!";

  let orphans : Vertex.Set.t =
    Edge.Set.fold
      (fun edge vertexes ->
        vertexes
        |> Vertex.Set.add edge.value.source.vertex
        |> Vertex.Set.add edge.value.target)
      all
      (Vertex.Set.singleton Vertex.root)
    |> Edge.Set.fold
         (fun edge orphans -> Vertex.Set.remove edge.value.target orphans)
         live
  in

  Format.printf "orphans = ";
  Vertex.print_set orphans;
  Format.printf "%!";

  let deleted : Vertex.Set.t = Vertex.Set.remove Vertex.root orphans in

  Format.printf "deleted = ";
  Vertex.print_set deleted;
  Format.printf "%!";

  let (r, r_edges) : t * Edge.Set.t = reachable live multiparent Vertex.root in

  Format.printf "R = %s%!" (show r);

  let (mp, mp_edges) : t list * Edge.Set.t =
    let ts, edge_sets =
      Vertex.Set.elements multiparent
      |> List.map (fun vertex ->
             reachable live (Vertex.Set.remove vertex multiparent) vertex)
      |> List.split
    in
    (ts, edge_set_concat edge_sets)
  in

  Format.printf "MP = [%s]%!" (List.map show mp |> String.concat "; ");

  let (d, d_edges) : t list * Edge.Set.t =
    let ts, edge_sets =
      Vertex.Set.elements deleted
      |> List.map (reachable live multiparent)
      |> List.split
    in
    (ts, edge_set_concat edge_sets)
  in

  Format.printf "D = [%s]%!" (List.map show d |> String.concat "; ");

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
        (Edge.Set.diff live (edge_set_concat [ r_edges; d_edges; mp_edges ]))
        []
      |> List.split
    in
    (ts, edge_set_concat edge_sets)
  in

  Format.printf "SC = [%s]%!" (List.map show sc |> String.concat "; ");

  (r, d, mp, sc)

(*******************************************************************************
 * Unit Tests
 ******************************************************************************)

let%test "mk 0" =
  Format.printf "\n--\n%!";
  let t = mk Vertex.root Edge.Set.empty |> fst in
  let want = Con (Vertex.root, IndexMap.empty) in
  Format.printf "\nGOT  %s%!" (show t);
  Format.printf "\nWANT %s\n%!" (show want);
  t = want

let%test "mk 1" =
  Format.printf "\n--\n%!";
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
  Format.printf "\nGOT  %s%!" (show t);
  Format.printf "\nWANT %s\n%!" (show want);
  t = want

let%test "mk 2" =
  Format.printf "\n--\n%!";
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
  Format.printf "\nGOT  %s%!" (show t);
  Format.printf "\nWANT %s\n%!" (show want);
  t = want

let%test "mk 1.1" =
  Format.printf "\n--\n%!";
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
  Format.printf "\nGOT  %s%!" (show t);
  Format.printf "\nWANT %s\n%!" (show want);
  t = want

let%test "mk 1.2" =
  Format.printf "\n--\n%!";
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
  Format.printf "\nGOT  %s%!" (show t);
  Format.printf "\nWANT %s\n\n%!" (show want);
  t = want

let%test "mk 1.1.1" =
  Format.printf "\n--\n%!";
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
  Format.printf "\nGOT  %s%!" (show t);
  Format.printf "\nWANT %s\n\n%!" (show want);
  t = want

let%test "mk sc" =
  Format.printf "\n--\n%!";
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
  Format.printf "\nGOT  %s%!" (show t);
  Format.printf "\nWANT %s\n%!" (show want);
  t = want

let%test "mk mp" =
  Format.printf "\n--\n%!";
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
  Format.printf "\nGOT  %s%!" (show t);
  Format.printf "\nWANT %s\n%!" (show want);
  t = want
