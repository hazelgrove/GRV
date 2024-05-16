open Edge_state
open Edge.Map

type t = Edge_state.t Edge.Map.t

let empty = Edge.Map.empty
let add = add

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
(* TODO Document how these functions work in the PDF *)
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

(* TODO: Add tests to check if sexps work *)
let%test "sexp_of_t" =
  let root = Vertex.root in
  let root_cursor = Cursor.root in
  let edge = Edge.mk root_cursor root in
  let graph = add edge Edge_state.Created empty in
  let sexp = sexp_of_t graph in
  let expected_sexp =
    "((((id 1)(value((source((vertex((id 0)(value Root_root)))(position \
     Root_root_root)))(target((id 0)(value Root_root))))))Created))"
  in
  (* Print sexp *)
  Printf.printf "sexp: %s\n" (Sexplib.Sexp.to_string sexp);
  Sexplib.Sexp.to_string sexp = expected_sexp

let%test "t_of_sexp" =
  let root = Vertex.root in
  let root_cursor = Cursor.root in
  let edge = Edge.mk root_cursor root in
  let graph = add edge Edge_state.Created empty in
  let sexp = sexp_of_t graph in
  let expected_graph = t_of_sexp sexp in
  print_endline
    ("Sexp of graph" ^ Sexplib.Sexp.to_string (sexp_of_t expected_graph));
  expected_graph = graph
