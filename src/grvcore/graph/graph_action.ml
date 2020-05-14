type t = { edge : Edge.t; state : Edge_state.t } [@@deriving show]

let pp (fmt : Format.formatter) (edge_action : t) : unit =
  let edge = Uuid.Wrap.unmk edge_action.edge in
  Format.fprintf fmt "%a (%a -> %s) [%s]" Edge_state.pp edge_action.state
    Cursor.pp edge.source
    (Uuid.Id.show edge.target.id)
    (Uuid.Id.show edge_action.edge.id)

(* TODO: review this carefully (note only parents/children maps updated) *)
let connect_parents (edge : Edge.t) (graph : Graph.t) : Graph.t =
  let target = Edge.target edge in
  let parents = Cache.parents target graph.cache in
  let parents =
    Vertex.Map.add target (Edge.Set.add edge parents) graph.cache.parents
  in
  { graph with cache = { graph.cache with parents } }

let disconnect_parents (edge : Edge.t) (graph : Graph.t) : Graph.t =
  let target = Edge.target edge in
  let parents = Cache.parents target graph.cache in
  let parents =
    Vertex.Map.add target
      (Edge.Set.filter (Edge.equal edge) parents)
      graph.cache.parents
  in
  { graph with cache = { graph.cache with parents } }

let connect_children (edge : Edge.t) (graph : Graph.t) : Graph.t =
  let source = Edge.source edge in
  let children = Cache.children source graph.cache in
  let children =
    Cursor.Map.add source (Edge.Set.add edge children) graph.cache.children
  in
  { graph with cache = { graph.cache with children } }

let disconnect_children (edge : Edge.t) (graph : Graph.t) : Graph.t =
  let source = Edge.source edge in
  let children = Cache.children source graph.cache in
  let children =
    Cursor.Map.add source
      (Edge.Set.filter (Edge.equal edge) children)
      graph.cache.children
  in
  { graph with cache = { graph.cache with children } }

let add_edge (graph : Graph.t) (edge : Edge.t) : Graph.t =
  graph |> connect_parents edge |> connect_children edge

let drop_edge (graph : Graph.t) (edge : Edge.t) : Graph.t =
  graph |> disconnect_parents edge |> disconnect_children edge

let apply (edge_action : t) (graph : Graph.t) : Graph.t =
  let edge = edge_action.edge in
  let edge_state = edge_action.state in
  let edges = Uuid.Map.add edge.id edge graph.edges in
  let vertices =
    let target = Edge.target edge in
    let source = Edge.source edge in
    graph.cache.vertices
    |> Uuid.Map.add target.id target
    |> Uuid.Map.add source.vertex.id source.vertex
  in
  let old_state = Edge.Map.find_opt edge graph.states in
  let action : Edge_state.t option =
    match old_state with
    | Some Destroyed -> None
    | Some Created -> (
        match edge_state with Destroyed -> Some Destroyed | Created -> None )
    | None -> Some edge_state
  in
  match action with
  | None -> graph
  | Some Created ->
      (* TODO: assert not already exists? *)

      (* TODO: short circuit if deleting a non-existant *)
      let states = Edge.Map.add edge Edge_state.Created graph.states in
      let cache = { graph.cache with vertices } in
      add_edge (Graph.mk edges states cache) edge
  | Some Destroyed -> (
      let states : Edge_state.t Edge.Map.t =
        Edge.Map.add edge Edge_state.Destroyed graph.states
      in
      match old_state with
      | None -> Graph.mk edges states { graph.cache with vertices }
      | _ ->
          let cache = { graph.cache with vertices } in
          drop_edge (Graph.mk edges states cache) edge )
