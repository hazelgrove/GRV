module Dom_html = Js_of_ocaml.Dom_html
module Vdom = Virtual_dom.Vdom

let ctrl (model : Model.t) (editor : Editor.t) (tabindexes : int Uuid.Map.t)
    (event : Dom_html.keyboardEvent Js.t) : Action.t' Option.t =
  match Dom_html.Keyboard_code.of_event event with
  | KeyS -> (
      match Gui.send model editor with
      | None ->
          Js_of_ocaml.Dom.preventDefault event;
          Js_of_ocaml.Dom_html.stopPropagation event;
          None
      | result -> result )
  | key -> (
      match key with
      | ArrowRight ->
          Js_of_ocaml.Dom.preventDefault event;
          Js_of_ocaml.Dom_html.stopPropagation event;
          Js.focus
            ( "editor"
            ^ Int.to_string
                ( match Uuid.Map.find_opt editor.id tabindexes with
                | Some tabindex ->
                    if tabindex = snd (Uuid.Map.max_binding tabindexes) then 1
                    else tabindex + 1
                | None -> 1 ) );
          None
      | ArrowLeft ->
          Js_of_ocaml.Dom.preventDefault event;
          Js_of_ocaml.Dom_html.stopPropagation event;
          Js.focus
            ( "editor"
            ^ Int.to_string
                ( match Uuid.Map.find_opt editor.id tabindexes with
                | Some tabindex ->
                    if tabindex = 1 then snd (Uuid.Map.max_binding tabindexes)
                    else tabindex - 1
                | None -> 1 ) );
          None
      | _ -> None )

let shift (event : Dom_html.keyboardEvent Js.t) : Action.t' Option.t =
  let%bind.Util.Option jstr : Js.js_string Js.t Option.t =
    Js.Optdef.to_option event##.key
  in
  match Js.to_string jstr with
  | "N" -> Some (Action.Edit (Create Typ_num))
  | "+" -> Some (Action.Edit (Create Exp_plus))
  | ">" -> Some (Action.Edit (Create Typ_arrow))
  | _ -> None

let base (editor : Editor.t) (event : Dom_html.keyboardEvent Js.t) :
    Action.t' Option.t =
  let id = Uuid.Id.show editor.id in
  match Dom_html.Keyboard_code.of_event event with
  | KeyN -> (
      Js_of_ocaml.Dom.preventDefault event;
      Js_of_ocaml.Dom_html.stopPropagation event;
      match Js.focus_input ("num_id" ^ id) with
      | "" -> None
      | str -> Some (Action.Edit (Create (Exp_num (int_of_string str)))) )
  | KeyP -> (
      match Js.focus_input ("pat_id" ^ id) with
      | "" ->
          Js_of_ocaml.Dom.preventDefault event;
          Js_of_ocaml.Dom_html.stopPropagation event;
          None
      | str -> Some (Edit (Create (Pat_var str))) )
  | KeyV -> (
      match Js.focus_input ("var_id" ^ id) with
      | "" ->
          Js_of_ocaml.Dom.preventDefault event;
          Js_of_ocaml.Dom_html.stopPropagation event;
          None
      | str -> Some (Edit (Create (Exp_var str))) )
  | Space -> Some (Edit (Create Exp_app))
  | Backslash -> Some (Edit (Create Exp_lam))
  | Delete ->
      Js.clear_selection ("deleted" ^ id);
      Some (Edit Destroy)
  | ArrowUp -> Some (Move Up)
  | ArrowDown -> Some (Move Down)
  | ArrowLeft -> Some (Move Left)
  | ArrowRight -> Some (Move Right)
  | _ -> None

let dispatch ~(inject : Action.t -> Vdom.Event.t) (model : Model.t)
    (editor : Editor.t) (tabindexes : int Uuid.Map.t)
    (event : Dom_html.keyboardEvent Js.t) : Vdom.Event.t =
  let handle =
    match
      Js.
        ( to_bool event##.shiftKey,
          to_bool event##.ctrlKey,
          to_bool event##.altKey )
    with
    | false, false, false -> base editor
    | true, false, false -> shift
    | false, true, false -> ctrl model editor tabindexes
    | _, _, _ -> fun _ -> (None : Action.t' Option.t)
  in
  match handle event with
  | Some action ->
      Js_of_ocaml.Dom.preventDefault event;
      Js_of_ocaml.Dom_html.stopPropagation event;
      inject { editor_id = editor.id; action }
  | None -> Vdom.Event.Ignore
