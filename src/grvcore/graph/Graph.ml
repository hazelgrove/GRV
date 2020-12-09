type t = Edge_state.t Edge.Map.t

let empty : t = Edge.Map.empty

(* Edge Queries *)

let edges (graph : t) : Edge.Set.t =
  Edge.Map.bindings graph |> List.map fst |> Edge.Set.of_list

let live_edges (graph : t) : Edge.Set.t =
  Edge.Map.bindings graph
  |> List.filter Edge_state.(function _, Created -> true | _ -> false)
  |> List.map fst |> Edge.Set.of_list

let parent_edges (graph : t) (vertex : Vertex.t) : Edge.Set.t =
  live_edges graph |> Edge.Set.filter (fun edge -> edge.value.target = vertex)

let child_edges (graph : t) (vertex : Vertex.t) (position : Lang.Position.t) :
    Edge.Set.t =
  live_edges graph
  |> Edge.Set.filter (fun edge ->
         edge.value.source = Cursor.{ vertex; position })

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
  Util.Sexp.of_map (Edge.Map.bindings graph) Edge.sexp_of_t Edge_state.sexp_of_t

let t_of_sexp (sexp : Sexplib.Sexp.t) : t =
  sexp
  |> Sexplib.Std.list_of_sexp (function
       | List [ key_sexp; value_sexp ] ->
           (Edge.t_of_sexp key_sexp, Edge_state.t_of_sexp value_sexp)
       | _ -> failwith __LOC__)
  |> List.to_seq |> Edge.Map.of_seq
