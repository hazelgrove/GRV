type t' = { edge : Edge.t; state : Edge_state.t } [@@deriving sexp, show]

type t = t' Uuid.Wrap.t [@@deriving sexp]

open Sexplib0.Sexp_conv

type sequence = t list [@@deriving sexp]

let pp (fmt : Format.formatter) (edge_action : t) : unit =
  let edge = Uuid.Wrap.unmk edge_action.value.edge in
  Format.fprintf fmt "%a %a → %s" Edge_state.pp edge_action.value.state
    Cursor.pp edge.source
    (Uuid.Id.show edge.target.id)

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
