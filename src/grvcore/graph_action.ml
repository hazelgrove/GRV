type t = { edge : Edge.t; state : Edge.state } [@@deriving show]

(* TODO: review this carefully (note only parents/children maps updated) *)
let connect_parents (edge : Edge.t) (graph : Graph.t) : Graph.t =
  let target = Edge.target edge in
  let parents = Graph.Parents.find target graph.parents in
  let parents =
    Graph.Parents.add target (Edge.Set.add edge parents) graph.parents
  in
  { graph with parents }

let disconnect_parents (edge : Edge.t) (graph : Graph.t) : Graph.t =
  let target = Edge.target edge in
  let parents = Graph.Parents.find target graph.parents in
  let parents =
    Graph.Parents.add target
      (Edge.Set.filter (Edge.equal edge) parents)
      graph.parents
  in
  { graph with parents }

let connect_children (edge : Edge.t) (graph : Graph.t) : Graph.t =
  let source = Graph.child (Edge.source edge) (Edge.index edge) in
  let children = Graph.Children.find source graph.children in
  let children =
    Graph.Children.add source (Edge.Set.add edge children) graph.children
  in
  { graph with children }

let disconnect_children (edge : Edge.t) (graph : Graph.t) : Graph.t =
  let source = Graph.child (Edge.source edge) (Edge.index edge) in
  let children = Graph.Children.find source graph.children in
  let children =
    Graph.Children.add source
      (Edge.Set.filter (Edge.equal edge) children)
      graph.children
  in
  { graph with children }

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
    graph.vertices
    |> Uuid.Map.add target.id target
    |> Uuid.Map.add source.id source
  in
  let old_state = Edge.Map.find_opt edge graph.states in
  let action : Edge.state option =
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
      let states = Edge.Map.add edge Edge.Created graph.states in
      add_edge { graph with edges; vertices; states } edge
  | Some Destroyed -> (
      let states : Edge.state Edge.Map.t =
        Edge.Map.add edge Edge.Destroyed graph.states
      in
      match old_state with
      | None -> { graph with edges; vertices; states }
      | _ -> drop_edge { graph with edges; vertices; states } edge )
