type t = { edge : Edge.t; state : Edge_state.t } [@@deriving sexp]

let to_string (edge_action : t) : string =
  Edge_state.to_string edge_action.state
  ^ " "
  ^ Cursor.to_string edge_action.edge.value.source
  ^ " → "
  ^ Uuid.Id.to_string edge_action.edge.value.target.id

let apply (action : t) (graph : Graph.t) : Graph.t =
  let old_state = Edge.Map.find_opt action.edge graph in
  let new_state = action.state in
  match (old_state, new_state) with
  | Some Deleted, _ -> graph
  | Some Created, Created -> graph
  | (Some Created | None), Deleted ->
      Edge.Map.add action.edge Edge_state.Deleted graph
  | None, Created ->
      (* TODO: assert not already exists? *)
      (* TODO: short circuit if deleting a non-existant *)
      Edge.Map.add action.edge Edge_state.Created graph

module OrderedType = struct
  type nonrec t = t

  let compare = compare
end

module Map = Map.Make (OrderedType)
module Set = Set.Make (OrderedType)
