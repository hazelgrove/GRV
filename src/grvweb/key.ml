module Dom_html = Js_of_ocaml.Dom_html
module Vdom = Virtual_dom.Vdom

let send (actions : Graph_action.t list) : Action.app Option.t =
  match Js.get_selection "actions" with
  | [] -> None
  | selection ->
      let actions = List.(map fst (filter snd @@ combine actions selection)) in
      Some (Send actions)

let ctrl (model : Model.t) (this_model : Model.Instance.t)
    (event : Dom_html.keyboardEvent Js.t) : Action.app Option.t =
  match Dom_html.Keyboard_code.of_event event with
  | KeyS ->
      Js_of_ocaml.Dom.preventDefault event;
      Js_of_ocaml.Dom_html.stopPropagation event;
      send this_model.actions
  | key ->
      let%map.Option action : Action.local Option.t =
        let refocus (next_id : int) (default_id : int) : unit =
          Js.focus_instance
            ( match Model.MapInt.find_opt next_id model with
            | Some _ -> next_id
            | None -> default_id )
        in
        match key with
        | ArrowLeft ->
            refocus (this_model.id - 1)
              (snd @@ Model.MapInt.max_binding model).id;
            None
        | ArrowRight ->
            refocus (this_model.id + 1)
              (snd @@ Model.MapInt.min_binding model).id;
            None
        | _ -> None
      in
      Action.Enqueue action

let shift (event : Dom_html.keyboardEvent Js.t) : Action.app Option.t =
  let%bind.Option jstr : Js.js_string Js.t Option.t =
    Js.Optdef.to_option event##.key
  in
  let%map.Option action : Action.local Option.t =
    match Js.to_string jstr with
    | "N" -> Some (Edit (Create Typ_num))
    | "+" -> Some (Edit (Create Exp_plus))
    | ">" -> Some (Edit (Create Typ_arrow))
    | _ -> None
  in
  Action.Enqueue action

let base (event : Dom_html.keyboardEvent Js.t) : Action.app Option.t =
  let%map.Option action : Action.local Option.t =
    match Dom_html.Keyboard_code.of_event event with
    | KeyN -> (
        Js_of_ocaml.Dom.preventDefault event;
        Js_of_ocaml.Dom_html.stopPropagation event;
        match Js.focus_input "num_id" with
        | "" -> None
        | str -> Some (Action.Edit (Create (Exp_num (int_of_string str)))) )
    | KeyP -> (
        match Js.focus_input "pat_id" with
        | "" ->
            Js_of_ocaml.Dom.preventDefault event;
            Js_of_ocaml.Dom_html.stopPropagation event;
            None
        | str -> Some (Edit (Create (Pat_var str))) )
    | KeyV -> (
        match Js.focus_input "var_id" with
        | "" ->
            Js_of_ocaml.Dom.preventDefault event;
            Js_of_ocaml.Dom_html.stopPropagation event;
            None
        | str -> Some (Edit (Create (Exp_var str))) )
    | Space -> Some (Edit (Create Exp_app))
    | Backslash -> Some (Edit (Create Exp_lam))
    | Delete -> Some (Edit Destroy)
    | ArrowUp -> Some (Move Out)
    | ArrowDown -> Some (Move In)
    | ArrowLeft -> Some (Move Left)
    | ArrowRight -> Some (Move Right)
    | _ -> None
  in
  Action.Enqueue action

let dispatch ~(inject : Action.t -> Vdom.Event.t) (model : Model.t)
    (this_model : Model.Instance.t) :
    Dom_html.keyboardEvent Js.t -> Vdom.Event.t =
 fun event ->
  let handle =
    match
      Js.
        ( to_bool event##.shiftKey,
          to_bool event##.ctrlKey,
          to_bool event##.altKey )
    with
    | false, false, false -> base
    | true, false, false -> shift
    | false, true, false -> ctrl model this_model
    | _, _, _ -> fun _ -> (None : Action.app Option.t)
  in
  match handle event with
  | Some action ->
      Js_of_ocaml.Dom.preventDefault event;
      Js_of_ocaml.Dom_html.stopPropagation event;
      inject { instance_id = this_model.id; action }
  | None -> Vdom.Event.Ignore
