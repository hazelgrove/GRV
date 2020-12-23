open Edge_state
open Edge.Map

type t = Edge_state.t Edge.Map.t

let empty = Edge.Map.empty

let apply_action (graph : t) (action : Graph_action.t) : t =
  let old_state = find_opt action.edge graph in
  let new_state = action.state in
  match (old_state, new_state) with
  | Some Deleted, _ -> graph
  | Some Created, Created -> graph
  | (Some Created | None), Deleted -> add action.edge Edge_state.Deleted graph
  | None, Created ->
      (* TODO: assert not already exists? *)
      (* TODO: short circuit if deleting a non-existant *)
      add action.edge Edge_state.Created graph

(* Edge Queries *)

let edges (graph : t) : Edge.Set.t =
  bindings graph |> List.map fst |> Edge.Set.of_list

let live_edges (graph : t) : Edge.Set.t =
  bindings graph
  |> List.filter (function _, Created -> true | _ -> false)
  |> List.map fst |> Edge.Set.of_list

let parent_edges (graph : t) (vertex : Vertex.t) : Edge.Set.t =
  live_edges graph |> Edge.Set.filter (fun edge -> edge.value.target = vertex)

let child_edges (graph : t) (vertex : Vertex.t) (position : Lang.Position.t) :
    Edge.Set.t =
  live_edges graph
  |> Edge.Set.filter (fun edge ->
         edge.value.source = Cursor.{ vertex; position })

(* Vertex Queries *)

let (vertexes, live_vertexes) : (t -> Vertex.Set.t) * (t -> Vertex.Set.t) =
  let impl (edge_source : t -> Edge.Set.t) : t -> Vertex.Set.t =
   fun graph ->
    Vertex.Set.empty
    |> Edge.Set.fold
         (fun edge vertexes ->
           vertexes
           |> Vertex.Set.add edge.value.source.vertex
           |> Vertex.Set.add edge.value.target)
         (edge_source graph)
  in
  (impl edges, impl live_edges)

let parent_vertexes (graph : t) (vertex : Vertex.t) : Vertex.Set.t =
  Vertex.Set.empty
  |> Edge.Set.fold
       (fun e -> Vertex.Set.add e.value.source.vertex)
       (parent_edges graph vertex)

let orphaned (graph : t) : Vertex.Set.t =
  vertexes graph
  |> Edge.Set.fold
       (fun edge -> Vertex.Set.remove edge.value.target)
       (live_edges graph)

let deleted (graph : t) : Vertex.Set.t =
  orphaned graph |> Vertex.Set.remove Vertex.root

let multiparented (graph : t) : Vertex.Set.t =
  (* find them all in one pass *)
  Vertex.Map.empty
  |> Edge.Set.fold
       (fun edge ->
         Vertex.Map.update edge.value.target (function
           | None -> Some 1
           | Some _ -> Some 2))
       (live_edges graph)
  |> Vertex.Map.filter (fun _ count -> count = 2)
  |> Vertex.Map.bindings |> List.map fst |> Vertex.Set.of_list

let vertex (graph : t) (vertex_id : Uuid.Id.t) : Vertex.t option =
  vertexes graph
  |> Vertex.Set.find_first_opt (fun vertex -> vertex.id = vertex_id)

(* S-Expression Conversions *)

let sexp_of_t (graph : t) : Sexplib.Sexp.t =
  Util.Sexp.of_map (bindings graph) Edge.sexp_of_t Edge_state.sexp_of_t

let t_of_sexp (sexp : Sexplib.Sexp.t) : t =
  sexp
  |> Sexplib.Std.list_of_sexp (function
       | List [ key_sexp; value_sexp ] ->
           (Edge.t_of_sexp key_sexp, Edge_state.t_of_sexp value_sexp)
       | _ -> failwith __LOC__)
  |> List.to_seq |> of_seq

(* Graph Decomposition *)

type subgraph = { roots : Vertex.Set.t; edges : Edge.Set.t }

let empty_subgraph = { roots = Vertex.Set.empty; edges = Edge.Set.empty }

type decomp = {
  multiparented : subgraph;
  deleted : subgraph;
  reachable : Edge.Set.t;
  cyclic : subgraph;
  parents : Edge.Set.t Vertex.Map.t;
  children : Edge.Set.t Vertex.Map.t;
}

let decompose (graph : t) : decomp =
  let live = live_edges graph in
  (* NOTE: degree-zero vertices are implicitly mapped to None *)
  let in_degrees =
    Vertex.Map.empty
    |> Edge.Set.fold
         (fun edge map ->
           map
           |> Vertex.Map.update edge.value.target (function
                | None -> Some 1
                | Some _ -> Some 2))
         live
  in
  (* NOTE: vertices with no parents are implicitly mapped to None *)
  let parents =
    Vertex.Map.empty
    |> Edge.Set.fold
         (fun edge ->
           Vertex.Map.update edge.value.target (function
             | None -> Some (Edge.Set.singleton edge)
             | Some edges -> Some (Edge.Set.add edge edges)))
         live
  in
  (* NOTE: vertices with no children are implicitly mapped to None *)
  let children =
    Vertex.Map.empty
    |> Edge.Set.fold
         (fun edge ->
           Vertex.Map.update edge.value.source.vertex (function
             | None -> Some (Edge.Set.singleton edge)
             | Some edges -> Some (Edge.Set.add edge edges)))
         live
  in
  let rec reachable_from ?(acc = Edge.Set.empty) (roots : Vertex.Set.t)
      (seen : Vertex.Set.t) : Edge.Set.t =
    print_endline
      ( "reachable_from " ^ Vertex.set_to_string roots ^ " "
      ^ Vertex.set_to_string seen ^ " " ^ Edge.set_to_string acc ^ " = " );
    let result =
      if Vertex.Set.is_empty roots then acc
      else
        let vertex = Vertex.Set.min_elt roots in
        match Vertex.Map.find_opt vertex children with
        | None -> Edge.Set.empty
        | Some child_edges ->
            print_endline ("child_edges = " ^ Edge.set_to_string child_edges);
            let child_vertexes =
              Vertex.Set.empty
              |> Edge.Set.fold
                   (fun edge -> Vertex.Set.add edge.value.target)
                   child_edges
            in
            reachable_from
              ~acc:(Edge.Set.union child_edges acc)
              Vertex.Set.(roots |> remove vertex |> union child_vertexes)
              Vertex.Set.(add vertex seen)
    in
    print_endline (Edge.set_to_string result);
    result
  in
  let multivertexes, univertexes, avertexes =
    (Vertex.Set.empty, Vertex.Set.empty, Vertex.Set.empty)
    |> Edge.Set.fold
         (fun edge (multi, uni, no) ->
           match Vertex.Map.find_opt edge.value.target in_degrees with
           | Some 2 -> (Vertex.Set.add edge.value.target multi, uni, no)
           | Some 1 -> (multi, Vertex.Set.add edge.value.target uni, no)
           | None ->
               ( multi,
                 uni,
                 if edge.value.target = Vertex.root then no
                 else Vertex.Set.add edge.value.target no )
           | _ -> assert false)
         live
    |> Edge.Set.fold
         (fun edge (multi, uni, no) ->
           match Vertex.Map.find_opt edge.value.source.vertex in_degrees with
           | Some 2 -> (Vertex.Set.add edge.value.source.vertex multi, uni, no)
           | Some 1 -> (multi, Vertex.Set.add edge.value.source.vertex uni, no)
           | None ->
               ( multi,
                 uni,
                 if edge.value.source.vertex = Vertex.root then no
                 else Vertex.Set.add edge.value.source.vertex no )
           | _ -> assert false)
         live
  in
  let enforest (roots : Vertex.Set.t) (seen : Vertex.Set.t)
      (remaining : Vertex.Set.t) : subgraph * Vertex.Set.t =
    print_endline
      ( "enforest " ^ Vertex.set_to_string roots ^ " "
      ^ Vertex.set_to_string seen ^ " "
      ^ Vertex.set_to_string remaining );
    let edges = reachable_from roots seen in
    let vertexes =
      Vertex.Set.empty
      |> Edge.Set.fold (fun edge -> Vertex.Set.add edge.value.target) edges
    in
    ({ roots; edges }, Vertex.Set.(diff remaining vertexes))
  in
  let multiparented, remaining =
    enforest multivertexes Vertex.Set.empty univertexes
  in
  let deleted, remaining = enforest avertexes multivertexes remaining in
  let reachables, _remaining =
    enforest (Vertex.Set.singleton Vertex.root) multivertexes remaining
  in
  {
    multiparented;
    deleted;
    reachable = reachables.edges;
    cyclic = empty_subgraph;
    parents;
    children;
  }

(* Unit Tests *)

let print_results = true

let report_vertex_set (prefix : string) (vertexes : Vertex.Set.t) : unit =
  print_string ("\n" ^ prefix ^ ":");
  Vertex.Set.iter
    (fun vertex -> print_string (" " ^ Uuid.Id.to_string vertex.id))
    vertexes

let report_edge_set (prefix : string) (edges : Edge.Set.t) : unit =
  print_string ("\n" ^ prefix ^ ":");
  Edge.Set.iter
    (fun edge ->
      print_string
        ( " ("
        ^ Uuid.Id.to_string edge.value.source.vertex.id
        ^ " -> "
        ^ Uuid.Id.to_string edge.value.target.id
        ^ ":"
        ^ Lang.Position.short_name edge.value.source.position
        ^ ")" ))
    edges

let report_map (prefix : string) (iter_prefix : string)
    (iter_id : Edge.t -> Uuid.Id.t * Lang.Position.t)
    (map : Edge.Set.t Vertex.Map.t) : unit =
  print_string ("\n" ^ prefix ^ ":");
  Vertex.Map.iter
    (fun vertex edges ->
      print_string (" (" ^ Uuid.Id.to_string vertex.id ^ " " ^ iter_prefix);
      edges
      |> Edge.Set.iter (fun edge ->
             let id, position = iter_id edge in
             print_string
               ( " " ^ Uuid.Id.to_string id ^ ":"
               ^ Lang.Position.short_name position ));
      print_string ")")
    map

let report_subgraph (prefix : string) (subgraph : subgraph) : unit =
  print_string ("\n" ^ prefix ^ ":");
  report_vertex_set "  V" subgraph.roots;
  report_edge_set "  E" subgraph.edges

let check_decompose ?(multiparented = empty_subgraph)
    ?(deleted = empty_subgraph) ?(reachable = Edge.Set.empty)
    ?(cyclic = empty_subgraph) ?(parents = Vertex.Map.empty)
    ?(children = Vertex.Map.empty) (graph : t) : bool =
  let got = decompose graph in
  let want = { multiparented; deleted; reachable; cyclic; parents; children } in
  got = want
  ||
  ( if print_results then (
      print_string "\n\n--\nGOT:";
      report_subgraph "MP" got.multiparented;
      report_subgraph "D" got.deleted;
      report_edge_set "RR" got.reachable;
      report_subgraph "SC" got.cyclic;
      report_map "P" "<-"
        (fun edge -> (edge.value.source.vertex.id, edge.value.source.position))
        got.parents;
      report_map "C" "->"
        (fun edge -> (edge.value.target.id, edge.value.source.position))
        got.children;
      print_string "\nWANT:";
      report_subgraph "MP" multiparented;
      report_subgraph "D" deleted;
      report_edge_set "RR" reachable;
      report_subgraph "SC" cyclic;
      report_map "P" "<-"
        (fun edge -> (edge.value.source.vertex.id, edge.value.source.position))
        parents;
      report_map "C" "->"
        (fun edge -> (edge.value.target.id, edge.value.source.position))
        children );
    false )

let%test_module "Graph.decompose" =
  ( module struct
    open Lang.Constructor
    open Lang.Position

    (* let%test "empty graph" = check_decompose empty *)

    let v0 = Vertex.root

    let%test_module "one vertex" =
      ( module struct
        let v1 = Vertex.mk Exp_plus

        let e01 = Edge.mk Cursor.{ vertex = v0; position = Root_root_root } v1

        (*
        - one (non-root) vertex
        - one live edge (i.e., in Created state) reachable from the root
        - no multiparented, deleted, or cyclic subgraphs
        *)
        let%test "reachable" =
          check_decompose
            (empty |> add e01 Created)
            ~reachable:(Edge.Set.singleton e01)
            ~parents:Vertex.Map.(empty |> add v1 (Edge.Set.of_list [ e01 ]))
            ~children:Vertex.Map.(empty |> add v0 (Edge.Set.of_list [ e01 ]))
      end )
  end )

(*
observations:
- (children Vertex.root) is a subset of decomp.reachable

properties:
- multiparented.edges \ 
*)

(* let%test "decompose deleted" =
  (* there are two (non-root) vertices *)
  let v1 = Vertex.mk Lang.Constructor.Exp_plus in
  let v2 = Vertex.mk Lang.Constructor.Exp_times in
  (* they are connected by a live edge *)
  let e12 = Edge.mk Cursor.{ vertex = v1; position = Exp_plus_left } v2 in
  let graph = empty |> add e12 Edge_state.Created in
  (* but the source vertex is not reachable from the root *)
  let deleted =
    { roots = Vertex.Set.(empty |> add v1); edges = Edge.Set.singleton e12 }
  in
  let parents = Vertex.Map.(empty |> add v2 (Edge.Set.singleton e12)) in
  let children = Vertex.Map.(empty |> add v1 (Edge.Set.singleton e12)) in
  check_decompose graph ~deleted ~parents ~children *)

(* let%test "decompose reachable" =
  (* there is only one (non-root) vertex *)
  let v1 = Vertex.mk Lang.Constructor.Exp_plus in
  (* and it is reachable from the root *)
  let e1 =
    Edge.mk Cursor.{ vertex = Vertex.root; position = Root_root_root } v1
  in
  let graph = add e1 Edge_state.Created empty in
  let reachable = Edge.Set.singleton e1 in
  let parents =
    Vertex.Map.(empty |> add e1.value.target (Edge.Set.singleton e1))
  in
  let children =
    Vertex.Map.(empty |> add e1.value.source.vertex (Edge.Set.singleton e1))
  in
  check_decompose graph ~reachable ~parents ~children *)

(* let%test "decompose multiparented" =
  (* there are two (non-root) vertexes *)
  let v1 = Vertex.mk Lang.Constructor.Exp_plus in
  let v2 = Vertex.mk Lang.Constructor.Exp_times in
  (* one is reachable from root *)
  let e01 =
    Edge.mk Cursor.{ vertex = Vertex.root; position = Root_root_root } v1
  in
  (* and twice-reachable from the other *)
  let e12 = Edge.mk Cursor.{ vertex = v1; position = Exp_plus_left } v2 in
  let e12' = Edge.mk Cursor.{ vertex = v1; position = Exp_plus_right } v2 in
  let graph =
    empty |> add e12 Edge_state.Created |> add e12' Edge_state.Created
  in
  let multiparented =
    { roots = Vertex.Set.singleton v2; edges = Edge.Set.of_list [ e12; e12' ] }
  in
  let reachable = Edge.Set.singleton e01 in
  let parents = Vertex.Map.(empty |> add v2 (Edge.Set.of_list [ e12; e12' ])) in
  let children =
    Vertex.Map.(empty |> add v1 (Edge.Set.of_list [ e12; e12' ]))
  in
  check_decompose graph ~multiparented ~reachable ~parents ~children *)

(* let%test "decompose multirooted" =
  (* there's only one (non-root) vertex *)
  let v1 = Vertex.mk Lang.Constructor.Exp_plus in
  (* and it is twice-reachable from the root *)
  let e1 =
    Edge.mk Cursor.{ vertex = Vertex.root; position = Root_root_root } v1
  in
  let e1' =
    Edge.mk Cursor.{ vertex = Vertex.root; position = Root_root_root } v1
  in
  let graph =
    (empty |> add e1 Edge_state.Created |> add e1' Edge_state.Created)
  in
  let multiparented =
    {
      vertexes =
        Vertex.Set.(empty |> add e1.value.target |> add e1'.value.target);
      edges = Edge.Set.of_list [ e1; e1' ];
    }
  in
  let parents = Vertex.Map.(empty |> add v1 (Edge.Set.of_list [ e1'; e1 ])) in
  let children =
    Vertex.Map.(empty |> add Vertex.root (Edge.Set.of_list [ e1'; e1 ]))
  in
  check_decompose graph ~multiparented ~parents ~children *)
