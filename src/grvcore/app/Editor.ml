open OptionUtil.Syntax
open Sexplib0.Sexp_conv

type id = Id.t [@@deriving sexp]

type t = {
  id : id;
  zgrove : ZGrove.t;
  local_actions : GraphAction.Set.t;
  global_actions : GraphAction.Set.t;
  show_ids : bool;
}
[@@deriving sexp]

type map = t Id.Map.t [@@deriving sexp]

let mk (u_gen : Id.Gen.t) : t * Id.Gen.t =
  let id, u_gen = Id.Gen.next u_gen in
  let zgrove = ZGrove.empty in
  let local_actions = GraphAction.Set.empty in
  let global_actions = GraphAction.Set.empty in
  ({ id; zgrove; local_actions; global_actions; show_ids = false }, u_gen)

let move (move_action : UserAction.move) (editor : t) : t option =
  let+ zgrove = ZGrove.move direction editor.zgrove in
  { editor with zgrove }

let edit (edit_action : UserAction.edit) (editor : t) : t option =
  let+ zgrove = ZGrove.edit edit_action editor.zgrove in
  { editor with zgrove }

(* let apply_graph_actions (editor : t) (graph_actions : GraphAction.t list) : t =
  let graph = Grove.recomp (ZGrove.erase_cursor editor.zgrove) in
  let graph = List.fold_left Graph.apply_graph_action graph graph_actions in
  let grove =  *)

let send (graph_actions : GraphAction.t list) (editor : t) : t =
  List.fold_left apply_graph_action editor graph_actions
