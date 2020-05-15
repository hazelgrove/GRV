type t = { edge : Edge.t; state : Edge_state.t } [@@deriving show]

let pp (fmt : Format.formatter) (edge_action : t) : unit =
  let edge = Uuid.Wrap.unmk edge_action.edge in
  Format.fprintf fmt "%a (%a -> %s) [%s]" Edge_state.pp edge_action.state
    Cursor.pp edge.source
    (Uuid.Id.show edge.target.id)
    (Uuid.Id.show edge_action.edge.id)

let apply (edge_action : t) (graph : Graph.t) : Graph.t =
  let edge = edge_action.edge in
  let edge_state = edge_action.state in
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
      {
        states;
        cache =
          { graph.cache with vertices }
          |> Cache.connect_parents edge
          |> Cache.connect_children edge;
      }
  | Some Destroyed -> (
      let states : Edge_state.t Edge.Map.t =
        Edge.Map.add edge Edge_state.Destroyed graph.states
      in
      match old_state with
      | None -> Graph.mk states { graph.cache with vertices }
      | _ ->
          {
            states;
            cache =
              { graph.cache with vertices }
              |> Cache.disconnect_parents edge
              |> Cache.disconnect_children edge;
          } )
