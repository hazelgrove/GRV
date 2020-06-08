(* Note: edges not in the map have not been created yet and are `\bot` *)
type t = Edge_state.t Edge.Map.t

let empty : t = Edge.Map.empty

let edges (graph : t) : Edge.Set.t =
  Edge.Set.of_list (List.map fst (Edge.Map.bindings graph))

let live_edges (graph : t) : Edge.Set.t =
  Edge.Map.fold
    (fun edge state edges ->
      if state = Edge_state.Created then Edge.Set.add edge edges else edges)
    graph Edge.Set.empty

let children (graph : t) (cursor : Cursor.t) : Edge.Set.t =
  Edge.Set.filter (fun edge -> edge.value.source = cursor) (live_edges graph)

let parents (graph : t) (vertex : Vertex.t) : Edge.Set.t =
  Edge.Set.filter (fun edge -> edge.value.target = vertex) (live_edges graph)

let vertexes (graph : t) : Vertex.Set.t =
  Edge.Set.fold
    (fun edge -> Vertex.Set.add edge.value.target)
    (edges graph)
    (Vertex.Set.singleton Vertex.root)

let vertex (graph : t) (vertex_id : Uuid.Id.t) : Vertex.t option =
  Vertex.Set.find_first_opt
    (fun vertex -> vertex.id = vertex_id)
    (vertexes graph)

let parent_vertexes (graph : t) (vertex : Vertex.t) : Vertex.Set.t =
  Edge.Set.fold
    (fun edge vertexes -> Vertex.Set.add edge.value.source.vertex vertexes)
    (parents graph vertex) Vertex.Set.empty

let orphans (graph : t) : Vertex.Set.t =
  Vertex.Set.filter
    (fun vertex -> Vertex.Set.is_empty (parent_vertexes graph vertex))
    (vertexes graph)

let seen (graph : t) : Vertex.Set.t =
  Vertex.Set.fold
    (fun vertex vertexes ->
      if Vertex.Set.subset (parent_vertexes graph vertex) vertexes then
        Vertex.Set.add vertex vertexes
      else vertexes)
    (vertexes graph) Vertex.Set.empty

let unseen (graph : t) : Vertex.Set.t =
  Vertex.Set.(diff (diff (vertexes graph) (orphans graph)) (seen graph))

let deleted (graph : t) : Vertex.Set.t =
  Vertex.Set.union
    (Vertex.Set.remove Vertex.root (orphans graph))
    (unseen graph)
