module Dom_html = Js_of_ocaml.Dom_html
module Js = Js_of_ocaml.Js

let ctrl (event : Dom_html.keyboardEvent Js.t) : Action.app Option.t =
  match Dom_html.Keyboard_code.of_event event with
  | KeyS -> Some Send
  | _ -> None

let shift (event : Dom_html.keyboardEvent Js.t) : Action.app Option.t =
  let%bind.Option jstr : Js.js_string Js.t Option.t =
    Js.Optdef.to_option event##.key
  in
  let%map.Option action : Action.local Option.t =
    match Js.to_string jstr with
    | "+" -> Some (Edit (Create Exp_plus))
    | ">" -> Some (Edit (Create Typ_arrow))
    | _ -> None
  in
  Action.Enqueue action

let base (event : Dom_html.keyboardEvent Js.t) : Action.app Option.t =
  let%map.Option action : Action.local Option.t =
    match Dom_html.Keyboard_code.of_event event with
    | KeyN -> Some (Edit (Create Typ_num))
    | KeyP -> Some (Edit (Create (Pat_var "P")))
    | KeyV -> Some (Edit (Create (Exp_var "X")))
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
