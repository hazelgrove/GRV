open EdgeState
open Old_Edge.Map

type t = EdgeState.t Old_Edge.Map.t

let empty = Old_Edge.Map.empty

let add = add

let apply_action (graph : t) (action : Graph_action.t) : t =
  let old_state = find_opt action.edge graph in
  let new_state = action.state in
  match (old_state, new_state) with
  | Some Minus, _ -> graph
  | Some Plus, Plus -> graph
  | (Some Plus | None), Minus -> add action.edge EdgeState.Minus graph
  | None, Plus ->
      (* TODO: assert not already exists? *)
      (* TODO: short circuit if deleting a non-existant *)
      add action.edge EdgeState.Plus graph

(* Edge Queries *)

let edges (graph : t) : Old_Edge.Set.t =
  bindings graph |> List.map fst |> Old_Edge.Set.of_list

let live_edges (graph : t) : Old_Edge.Set.t =
  bindings graph
  |> List.filter (function _, Plus -> true | _ -> false)
  |> List.map fst |> Old_Edge.Set.of_list

let parent_edges (graph : t) (vertex : Old_Vertex.t) : Old_Edge.Set.t =
  live_edges graph
  |> Old_Edge.Set.filter (fun edge -> edge.value.target = vertex)

let child_edges (graph : t) (vertex : Old_Vertex.t) (position : Lang.Position.t)
    : Old_Edge.Set.t =
  live_edges graph
  |> Old_Edge.Set.filter (fun edge ->
         edge.value.source = Cursor.{ vertex; position })

(* Vertex Queries *)

let (vertexes, live_vertexes) :
      (t -> Old_Vertex.Set.t) * (t -> Old_Vertex.Set.t) =
  let impl (edge_source : t -> Old_Edge.Set.t) : t -> Old_Vertex.Set.t =
   fun graph ->
    Old_Vertex.Set.empty
    |> Old_Edge.Set.fold
         (fun edge vertexes ->
           vertexes
           |> Old_Vertex.Set.add edge.value.source.vertex
           |> Old_Vertex.Set.add edge.value.target)
         (edge_source graph)
  in
  (impl edges, impl live_edges)

let parent_vertexes (graph : t) (vertex : Old_Vertex.t) : Old_Vertex.Set.t =
  Old_Vertex.Set.empty
  |> Old_Edge.Set.fold
       (fun e -> Old_Vertex.Set.add e.value.source.vertex)
       (parent_edges graph vertex)

let orphaned (graph : t) : Old_Vertex.Set.t =
  vertexes graph
  |> Old_Edge.Set.fold
       (fun edge -> Old_Vertex.Set.remove edge.value.target)
       (live_edges graph)

let deleted (graph : t) : Old_Vertex.Set.t =
  orphaned graph |> Old_Vertex.Set.remove Old_Vertex.root

let multiparented (graph : t) : Old_Vertex.Set.t =
  (* find them all in one pass *)
  Old_Vertex.Map.empty
  |> Old_Edge.Set.fold
       (fun edge ->
         Old_Vertex.Map.update edge.value.target (function
           | None -> Some 1
           | Some _ -> Some 2))
       (live_edges graph)
  |> Old_Vertex.Map.filter (fun _ count -> count = 2)
  |> Old_Vertex.Map.bindings |> List.map fst |> Old_Vertex.Set.of_list

let vertex (graph : t) (vertex_id : Uuid.Id.t) : Old_Vertex.t option =
  vertexes graph
  |> Old_Vertex.Set.find_first_opt (fun vertex -> vertex.id = vertex_id)

(* S-Expression Conversions *)

let sexp_of_t (graph : t) : Sexplib.Sexp.t =
  Util.Sexp.of_map (bindings graph) Old_Edge.sexp_of_t EdgeState.sexp_of_t

let t_of_sexp (sexp : Sexplib.Sexp.t) : t =
  sexp
  |> Sexplib.Std.list_of_sexp (function
       | List [ key_sexp; value_sexp ] ->
           (Old_Edge.t_of_sexp key_sexp, EdgeState.t_of_sexp value_sexp)
       | _ -> failwith __LOC__)
  |> List.to_seq |> of_seq
