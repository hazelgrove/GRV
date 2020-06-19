open Sexplib0.Sexp_conv

type graph_action_sequence = (Uuid.Id.t * Graph_action.t) list [@@deriving sexp]

(* TODO: add a field for global action history *)
type t = {
  editors : Editor.t Uuid.Map.t;
  actions : graph_action_sequence option;
}

let sexp_of_editors (editors : Editor.t Uuid.Map.t) : Sexplib.Sexp.t =
  Sexplib.Std.sexp_of_list
    (fun (id, editor) ->
      Sexplib.Sexp.List [ Uuid.Id.sexp_of_t id; Editor.sexp_of_t editor ])
    (Uuid.Map.bindings editors)

let editors_of_sexp (sexp : Sexplib.Sexp.t) : Editor.t Uuid.Map.t =
  let bindings =
    Sexplib.Std.list_of_sexp
      (function
        | Sexplib.Sexp.List [ id_sexp; editor_sexp ] ->
            (Uuid.Id.t_of_sexp id_sexp, Editor.t_of_sexp editor_sexp)
        | _ -> failwith __LOC__)
      sexp
  in
  Uuid.Map.of_seq (List.to_seq bindings)

let sexp_of_t (model : t) : Sexplib.Sexp.t =
  Sexplib.Sexp.List
    [
      sexp_of_editors model.editors;
      sexp_of_option sexp_of_graph_action_sequence model.actions;
    ]

let t_of_sexp : Sexplib.Sexp.t -> t = function
  | List [ editors_sexp; actions_sexp ] ->
      let editors = editors_of_sexp editors_sexp in
      let actions =
        Sexplib.Std.option_of_sexp graph_action_sequence_of_sexp actions_sexp
      in
      { editors; actions }
  | _ -> failwith __LOC__

let empty : t =
  let editor1 = Editor.mk () in
  let editor2 = Editor.mk () in
  (* TODO: helper for this *)
  let editors =
    Uuid.Map.of_seq
      (List.to_seq [ (editor1.id, editor1); (editor2.id, editor2) ])
  in
  { editors; actions = Some [] }

let cutoff (m1 : t) (m2 : t) : bool = m1 == m2

let filter_editor_actions (globally_known : Graph_action.Set.t)
    (editor : Editor.t) : Editor.t =
  let actions =
    Graph_action.Set.filter
      (fun a -> not (Graph_action.Set.mem a globally_known))
      editor.actions
  in
  { editor with actions }

(* let globally_known_actions (model : t) : Graph_action.Set.t =
 *   Uuid.Map.fold
 *     (fun _ editor -> Graph_action.Set.union editor.actions)
 *     model Graph_action.Set.empty *)

let globally_known_actions (model : t) : Graph_action.Set.t =
  let knowns =
    List.map
      (fun (_, (e : Editor.t)) -> e.known_actions)
      (Uuid.Map.bindings model.editors)
  in
  List.fold_left Graph_action.Set.inter (List.hd knowns) knowns

let remove_known_actions (model : t) : t =
  let known_actions = globally_known_actions model in
  let editors =
    Uuid.Map.map (filter_editor_actions known_actions) model.editors
  in
  { model with editors }
