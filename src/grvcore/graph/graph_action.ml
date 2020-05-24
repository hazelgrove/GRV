type t = { edge : Edge.t; state : Edge_state.t } [@@deriving show]

let pp (fmt : Format.formatter) (edge_action : t) : unit =
  let edge = Uuid.Wrap.unmk edge_action.edge in
  Format.fprintf fmt "%a %a -> %s" Edge_state.pp edge_action.state Cursor.pp
    edge.source
    (Uuid.Id.show edge.target.id)

let apply (edge_action : t) (graph : Graph.t) : Graph.t =
  let edge = edge_action.edge in
  let edge_state = edge_action.state in
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
      { states; cache = Cache.create edge graph.cache }
  | Some Destroyed -> (
      let states : Edge_state.t Edge.Map.t =
        Edge.Map.add edge Edge_state.Destroyed graph.states
      in
      match old_state with
      | None -> Graph.mk states graph.cache
      | _ -> { states; cache = Cache.destroy edge graph.cache } )
