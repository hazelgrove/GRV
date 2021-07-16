open OptionUtil.Syntax
module Dom_html = Js_of_ocaml.Dom_html
module Vdom = Virtual_dom.Vdom

module InputAction = struct
  type t = Pass | Do of Action.t' option
end

let on_keydown (ctx : ViewContext.t) (key_evt : Dom_html.keyboardEvent Js.t) :
    Vdom.Event.t =
  let ctrl = Js.to_bool key_evt##.ctrlKey in
  let shift = Js.to_bool key_evt##.shiftKey in
  let key_opt =
    let+ jstr = key_evt##.code |> Js_of_ocaml.Js.Optdef.to_option in
    Js_of_ocaml.Js.to_string jstr
  in
  print_endline (Option.value ~default:"<??>" key_opt);
  let editor_opt = ViewContext.editor_opt ctx in
  let input_action =
    let open InputAction in
    match (ctrl, shift, key_opt) with
    | false, false, Some "KeyV" -> (
        match Js.prompt "var_id" with
        | "" -> Do None
        | x -> (
            match ViewContext.cursor_sort ctx with
            | Some Exp -> Do (Some (UserAction (Construct (ExpVar x))))
            | Some Pat -> Do (Some (UserAction (Construct (PatVar x))))
            | Some Typ | None -> Do None))
    | _, _, _ -> Pass
  in
  match (editor_opt, input_action) with
  | None, _ | _, Pass -> Vdom.Event.Ignore
  | Some editor, Do action_opt -> (
      let editor_cssid = "Editor" ^ Id.to_string editor.id in
      Js.claim_event key_evt;
      Js.focus editor_cssid;
      match action_opt with
      | None -> Vdom.Event.Ignore
      | Some action -> ctx.inject Action.{ editor_id = editor.id; action })
