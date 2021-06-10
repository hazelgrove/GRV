type t = { edge : Old_Edge.t; state : Old_Edge_state.t } [@@deriving sexp]

let to_string (edge_action : t) : string =
  Format.sprintf "%s %s → %s"
    (Old_Edge_state.to_string edge_action.state)
    (Cursor.to_string edge_action.edge.value.source)
    (Uuid.Id.to_string edge_action.edge.value.target.id)

module OrderedType = struct
  type nonrec t = t

  let compare = compare
end

module Map = Map.Make (OrderedType)
module Set = Set.Make (OrderedType)
