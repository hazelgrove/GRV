module Dom_html = Js_of_ocaml.Dom_html

let focus_input (id : string) (this_model : Model.Instance.t) : string Option.t
    =
  Js.eval_to_unit @@ "refocus('" ^ id ^ "')";
  match Js.eval_to_string @@ "getInput('" ^ id ^ "')" with
  | "" -> None
  | str ->
      Js.eval_to_unit @@ "setInput('" ^ id ^ "', '')";
      Js.eval_to_unit ("refocus('instance" ^ string_of_int this_model.id ^ "')");
      Some str

let focus_instance (next_model_opt : Model.Instance.t Option.t)
    (default_model : Model.Instance.t) : unit =
  let next_id =
    match next_model_opt with
    | Some next_model -> next_model.id
    | None -> default_model.id
  in
  Js.eval_to_unit ("refocus('instance" ^ string_of_int next_id ^ "')")

let ctrl (model : Model.t) (this_model : Model.Instance.t)
    (event : Dom_html.keyboardEvent Js.t) : Action.app Option.t =
  match Dom_html.Keyboard_code.of_event event with
  | KeyS -> Some Send
  | key ->
      let%map.Option action : Action.local Option.t =
        match key with
        | KeyP -> (
            match focus_input "pat_id" this_model with
            | None ->
                Js_of_ocaml.Dom.preventDefault event;
                Js_of_ocaml.Dom_html.stopPropagation event;
                None
            | Some str -> Some (Edit (Create (Pat_var str))) )
        | KeyV -> (
            match focus_input "var_id" this_model with
            | None ->
                Js_of_ocaml.Dom.preventDefault event;
                Js_of_ocaml.Dom_html.stopPropagation event;
                None
            | Some str -> Some (Edit (Create (Exp_var str))) )
        | ArrowLeft ->
            focus_instance
              (Model.MapInt.find_opt (this_model.id - 1) model)
              (snd @@ Model.MapInt.max_binding model);
            None
        | ArrowRight ->
            focus_instance
              (Model.MapInt.find_opt (this_model.id + 1) model)
              (snd @@ Model.MapInt.min_binding model);
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
    | "+" -> Some (Edit (Create Exp_plus))
    | ">" -> Some (Edit (Create Typ_arrow))
    | _ -> None
  in
  Action.Enqueue action

let base (event : Dom_html.keyboardEvent Js.t) : Action.app Option.t =
  let%map.Option action : Action.local Option.t =
    match Dom_html.Keyboard_code.of_event event with
    | KeyN -> Some (Edit (Create Typ_num))
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
