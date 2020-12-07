type t' = { edge : Edge.t; state : Edge_state.t } [@@deriving sexp]

type t = t' Uuid.Wrap.t [@@deriving sexp]

let to_string (edge_action : t) : string =
  Edge_state.to_string edge_action.value.state
  ^ " "
  ^ Cursor.to_string edge_action.value.edge.value.source
  ^ " → "
  ^ Uuid.Id.show edge_action.value.edge.value.target.id

let apply (action : t) (graph : Graph.t) : Graph.t =
  let old_state = Edge.Map.find_opt action.value.edge graph in
  let new_state = action.value.state in
  match (old_state, new_state) with
  | Some Destroyed, _ -> graph
  | Some Created, Created -> graph
  | (Some Created | None), Destroyed ->
      Edge.Map.add action.value.edge Edge_state.Destroyed graph
  | None, Created ->
      (* TODO: assert not already exists? *)
      (* TODO: short circuit if deleting a non-existant *)
      Edge.Map.add action.value.edge Edge_state.Created graph

module OrderedType = struct
  type nonrec t = t

  let compare = Uuid.Wrap.compare
end

module Map = Map.Make (OrderedType)
module Set = Set.Make (OrderedType)
