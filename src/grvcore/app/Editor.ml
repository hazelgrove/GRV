(* open OptionUtil.Syntax *)
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

let apply_graph_actions (graph_actions : GraphAction.Set.t) (editor : t) : t =
  let graph, graph_cursor = ZGrove.recomp editor.zgrove in
  let graph, graph_cursor =
    Graph.apply_actions graph_actions graph graph_cursor
  in
  let zgrove = ZGrove.decomp graph graph_cursor in
  { editor with zgrove }

(* let move (move_action : UserAction.move) (editor : t) : t option =
  let+ zgrove = ZGrove.move move_action editor.zgrove in
  { editor with zgrove }

let edit (edit_action : UserAction.edit) (editor : t) (u_gen : Id.Gen.t) :
    (t * Id.Gen.t) option =
  let* graph_edits, u_gen = ZGrove.edit edit_action editor.zgrove u_gen in
  let path = ZGrove.follow_cursor editor.zgrove in
  let graph = ZGrove.recomp editor.zgrove |> Graph.apply_edits graph_edits in
  let grove = Grove.decomp graph in
  let+ zgrove = ZGrove.place_cursor path editor.zgrove in
  ({ editor with zgrove }, u_gen)

let send (graph_actions : GraphAction.t list) (editor : t) : t =
  List.fold_left apply_graph_action editor graph_actions *)
