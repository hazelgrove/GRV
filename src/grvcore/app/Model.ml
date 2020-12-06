open Sexplib0.Sexp_conv

type graph_action_sequence = (Uuid.Id.t * Graph_action.t) list [@@deriving sexp]

type t = {
  editors : Editor.t Uuid.Map.t;
  actions : graph_action_sequence option;
}

let sexp_of_t (model : t) : Sexplib.Sexp.t =
  Sexplib.(
    Sexp.List
      [
        Editor.sexp_of_map model.editors;
        sexp_of_option sexp_of_graph_action_sequence model.actions;
      ])

let t_of_sexp : Sexplib.Sexp.t -> t = function
  | List [ editors_sexp; actions_sexp ] ->
      let editors = Editor.map_of_sexp editors_sexp in
      let actions =
        Sexplib.Std.option_of_sexp graph_action_sequence_of_sexp actions_sexp
      in
      { editors; actions }
  | _ -> failwith __LOC__

let mk () : t =
  let editor1 = Editor.mk () in
  let editor2 = Editor.mk () in
  let editors =
    Uuid.Map.empty
    |> Uuid.Map.add editor1.id editor1
    |> Uuid.Map.add editor2.id editor2
  in
  { editors; actions = Some [] }

let cutoff (m1 : t) (m2 : t) : bool = m1 == m2

let filter_editor_actions (globally_known : Graph_action.Set.t)
    (editor : Editor.t) : Editor.t =
  let actions =
    editor.actions
    |> Graph_action.Set.filter (fun a ->
           not (Graph_action.Set.mem a globally_known))
  in
  { editor with actions }

let globally_known_actions (model : t) : Graph_action.Set.t =
  let knowns =
    Uuid.Map.bindings model.editors
    |> List.map snd
    |> List.map (fun Editor.{ known_actions; _ } -> known_actions)
  in
  List.fold_left Graph_action.Set.inter (List.hd knowns) knowns

let remove_known_actions (model : t) : t =
  let known_actions = globally_known_actions model in
  let editors =
    Uuid.Map.map (filter_editor_actions known_actions) model.editors
  in
  { model with editors }
