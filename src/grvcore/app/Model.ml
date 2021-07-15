open OptionUtil.Syntax
open Sexplib0.Sexp_conv

type t = {
  editors : Editor.t Id.Map.t;
  actions : GraphAction.Set.t option;
  u_gen : Id.Gen.t;
}
[@@deriving sexp]

let mk () : t =
  let u_gen = Id.Gen.init in
  let editor1, u_gen = Editor.mk u_gen in
  let editor2, u_gen = Editor.mk u_gen in
  let editors =
    List.to_seq [ (editor1.id, editor1); (editor2.id, editor2) ]
    |> Id.Map.of_seq
  in
  { editors; actions = Some GraphAction.Set.empty; u_gen }

let cutoff (m1 : t) (m2 : t) : bool = m1 == m2

let filter_editor_actions (globally_known : GraphAction.Set.t)
    (editor : Editor.t) : Editor.t =
  let local_actions =
    GraphAction.Set.filter
      (fun action -> not (GraphAction.Set.mem action globally_known))
      editor.local_actions
  in
  { editor with local_actions }

let globally_known_actions (model : t) : GraphAction.Set.t =
  let knowns =
    Id.Map.bindings model.editors
    |> List.map snd
    |> List.map (fun Editor.{ global_actions; _ } -> global_actions)
  in
  List.fold_left GraphAction.Set.inter (List.hd knowns) knowns

let remove_known_actions (model : t) : t =
  let known_actions = globally_known_actions model in
  let editors =
    Id.Map.map (filter_editor_actions known_actions) model.editors
  in
  { model with editors }

let update_editor (update : Editor.t -> Editor.t option) (editor : Editor.t)
    (model : t) : t option =
  let* editor = Id.Map.find_opt editor.id model.editors in
  let+ editor = update editor in
  let editors = Id.Map.add editor.id editor model.editors in
  { model with editors }

(* let update_editors (update : Editor.t -> Editor.t option)
    (editors : Editor.t list) (model : t) : t option =
  List.fold_left
    (fun model_opt editor ->
      let* model = model_opt in
      update_editor (Editor.send graph_actions) editor model)
    (Some model) editors *)

let get_editor (editor_id : Editor.id) (model : t) : Editor.t option =
  Id.Map.find_opt editor_id model.editors

let get_editors (editor_ids : Editor.id list) (model : t) : Editor.t list option
    =
  editor_ids
  |> List.map (fun editor_id -> get_editor editor_id model)
  |> OptionUtil.of_list
