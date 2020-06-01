type t' = { edge : Edge.t; state : Edge_state.t } [@@deriving sexp_of, show]

type t = t' Uuid.Wrap.t [@@deriving sexp_of]

let pp (fmt : Format.formatter) (edge_action : t) : unit =
  let edge = Uuid.Wrap.unmk edge_action.value.edge in
  Format.fprintf fmt "%a %a → %s" Edge_state.pp edge_action.value.state
    Cursor.pp edge.source
    (Uuid.Id.show edge.target.id)

let apply (action : t) (graph : Graph.t) : Graph.t =
  let old_state = Edge.Map.find_opt action.value.edge graph.states in
  let new_state = action.value.state in
  match (old_state, new_state) with
  | Some Destroyed, _ -> graph
  | Some Created, Created -> graph
  | (Some Created | None), Destroyed -> (
      let states : Edge_state.t Edge.Map.t =
        Edge.Map.add action.value.edge Edge_state.Destroyed graph.states
      in
      match old_state with
      | None -> Graph.mk states graph.cache
      | _ -> { states; cache = Cache.destroy action.value.edge graph.cache } )
  | None, Created ->
      (* TODO: assert not already exists? *)

      (* TODO: short circuit if deleting a non-existant *)
      let states =
        Edge.Map.add action.value.edge Edge_state.Created graph.states
      in
      { states; cache = Cache.create action.value.edge graph.cache }

module OrderedType = struct
  type nonrec t = t

  let compare = Uuid.Wrap.compare
end

module Map = Map.Make (OrderedType)
module Set = Set.Make (OrderedType)
