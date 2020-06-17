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

let vertex_children (graph : t) (vertex : Vertex.t) : Edge.Set.t =
  Edge.Set.filter
    (fun edge -> edge.value.source.vertex = vertex)
    (live_edges graph)

let cursor_children (graph : t) (cursor : Cursor.t) : Edge.Set.t =
  Edge.Set.filter (fun edge -> edge.value.source = cursor) (live_edges graph)

let parents (graph : t) (vertex : Vertex.t) : Edge.Set.t =
  Edge.Set.filter (fun edge -> edge.value.target = vertex) (live_edges graph)

let vertexes (graph : t) : Vertex.Set.t =
  Edge.Set.fold
    (fun edge vertexes ->
      Vertex.Set.add edge.value.source.vertex
        (Vertex.Set.add edge.value.target vertexes))
    (edges graph) Vertex.Set.empty

let vertex (graph : t) (vertex_id : Uuid.Id.t) : Vertex.t option =
  Vertex.Set.find_first_opt
    (fun vertex -> vertex.id = vertex_id)
    (vertexes graph)

let orphans (graph : t) : Vertex.Set.t =
  Edge.Set.fold
    (fun edge orphans -> Vertex.Set.remove edge.value.target orphans)
    (live_edges graph) (vertexes graph)

let multiparents (graph : t) : Vertex.Set.t =
  let count =
    Edge.Set.fold
      (fun edge count ->
        match Vertex.Map.find_opt edge.value.target count with
        | None -> Vertex.Map.add edge.value.target 1 count
        | Some _ -> Vertex.Map.add edge.value.target 2 count)
      (live_edges graph) Vertex.Map.empty
  in
  let two_counts : int Vertex.Map.t =
    Vertex.Map.filter (fun _ c -> c = 2) count
  in
  Vertex.Map.fold
    (fun vertex _ -> Vertex.Set.add vertex)
    two_counts Vertex.Set.empty

type roots = {
  root : Vertex.t;
  multiparent : Vertex.Set.t;
  deleted : Vertex.Set.t;
}

let roots (graph : t) : roots =
  (* First, find the vertexes with no parents *)
  let orphan_vertexes = orphans graph in
  let multiparent = multiparents graph in

  (* This set tracks the vertexes that are reachable from our selected roots *)
  let reachable = ref Vertex.Set.empty in
  (* This function add a vertex and everything reachable from it to the reachable set *)
  let rec add (vertex : Vertex.t) : unit =
    if Vertex.Set.mem vertex !reachable then ()
    else (
      reachable := Vertex.Set.add vertex !reachable;
      Edge.Set.iter
        (fun edge -> add edge.value.target)
        (vertex_children graph vertex) )
  in
  (* Mark everything reachable from true_orphans or multiparents as reachable *)
  Vertex.Set.iter add orphan_vertexes;
  Vertex.Set.iter add multiparent;

  (* Note that we rely on iter doing from least to greatest *)
  let deleted = ref (Vertex.Set.remove Vertex.root orphan_vertexes) in
  Vertex.Set.iter
    (fun vertex ->
      if Vertex.Set.mem vertex !reachable then ()
      else (
        deleted := Vertex.Set.add vertex !deleted;
        add vertex ))
    (vertexes graph);

  { root = Vertex.root; multiparent; deleted = !deleted }
