open OptionUtil.Syntax
module Dom_html = Js_of_ocaml.Dom_html
module Vdom = Virtual_dom.Vdom

let moveLR (editor : Editor.t) (tabindexes : int Id.Map.t)
    (event : Dom_html.keyboardEvent Js.t) (delta : int) : unit =
  Js.claim_event event;
  let editor_idx =
    match Id.Map.find_opt editor.id tabindexes with
    | Some tabindex -> (
        match Id.Map.max_binding_opt tabindexes with
        | None -> 1
        | Some binding -> if tabindex = snd binding then 1 else tabindex + delta
        )
    | None -> 1
  in
  Js.focus ("editor" ^ Int.to_string editor_idx)

let ctrl (model : Model.t) (editor : Editor.t) (tabindexes : int Id.Map.t)
    (event : Dom_html.keyboardEvent Js.t) : Action.t' option =
  match Dom_html.Keyboard_code.of_event event with
  | KeyS -> (
      match Gui.send model editor with
      | None ->
          Js.claim_event event;
          None
      | result -> result)
  | key -> (
      match key with
      | ArrowRight ->
          moveLR editor tabindexes event 1;
          None
      | ArrowLeft ->
          moveLR editor tabindexes event (-1);
          None
      | _ -> None)

let shift (event : Dom_html.keyboardEvent Js.t) : Action.t' option =
  let* jstr = Js.Optdef.to_option event##.key in
  match Js.to_string jstr with
  | "N" -> Some (Action.UserAction (Construct TypNum))
  | "+" -> Some (Action.UserAction (Construct ExpPlus))
  | "*" -> Some (Action.UserAction (Construct ExpTimes))
  | ">" -> Some (Action.UserAction (Construct TypArrow))
  | _ -> None

let base (editor : Editor.t) (event : Dom_html.keyboardEvent Js.t) :
    Action.t' option =
  let editor_id = Id.to_string editor.id in
  match Dom_html.Keyboard_code.of_event event with
  | KeyN -> (
      match Js.prompt "num_id" with
      | "" ->
          Js.claim_event event;
          None
      | str ->
          Js.focus ("editor" ^ editor_id);
          let+ num = int_of_string_opt str in
          Action.UserAction (Construct (ExpNum num)))
  | KeyP -> (
      match Js.prompt "pat_id" with
      | "" ->
          Js.claim_event event;
          None
      | str ->
          Js.focus ("editor" ^ editor_id);
          Some (UserAction (Construct (PatVar str))))
  | KeyV -> (
      match Js.prompt "var_id" with
      | "" ->
          Js.claim_event event;
          None
      | str ->
          Js.focus ("editor" ^ editor_id);
          Some (UserAction (Construct (ExpVar str))))
  | Space -> Some (UserAction (Construct ExpApp))
  | Backslash -> Some (UserAction (Construct ExpLam))
  | Delete ->
      Js.clear_selection ("deleted" ^ editor_id);
      Some (UserAction Delete)
  | ArrowUp -> Some (UserAction (Move Up))
  | ArrowDown -> Some (UserAction (Move Down))
  | ArrowLeft -> Some (UserAction (Move Left))
  | ArrowRight -> Some (UserAction (Move Right))
  | _ -> None

let dispatch ~(inject : Action.t -> Vdom.Event.t) (model : Model.t)
    (editor : Editor.t) (tabindexes : int Id.Map.t)
    (event : Dom_html.keyboardEvent Js.t) : Vdom.Event.t =
  let handle =
    match Js.(to_bool event##.shiftKey, to_bool event##.ctrlKey) with
    | false, false -> base editor
    | true, false -> shift
    | false, true -> ctrl model editor tabindexes
    | _, _ -> fun _ : Action.t' option -> None
  in
  match handle event with
  | Some action ->
      Js.claim_event event;
      Js.focus ("editor" ^ Id.to_string editor.id);
      inject { editor_id = editor.id; action }
  | None -> Vdom.Event.Ignore
