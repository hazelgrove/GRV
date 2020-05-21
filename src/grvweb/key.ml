module Dom_html = Js_of_ocaml.Dom_html

let ctrl (model : Model.t) (this_model : Model.Instance.t)
    (event : Dom_html.keyboardEvent Js.t) : Action.app Option.t =
  match Dom_html.Keyboard_code.of_event event with
  | KeyS -> Some Send
  | key ->
      let%map.Option action : Action.local Option.t =
        match key with
        | KeyP -> (
            Js.eval_to_unit "refocus('pat_id')";
            match Js.eval_to_string "getInput('pat_id')" with
            | "" ->
                Js_of_ocaml.Dom.preventDefault event;
                Js_of_ocaml.Dom_html.stopPropagation event;
                None
            | str ->
                Js.eval_to_unit "setInput('pat_id', '')";
                Js.eval_to_unit
                  ("refocus('instance" ^ string_of_int this_model.id ^ "')");
                Some (Edit (Create (Pat_var str))) )
        | KeyV -> (
            Js.eval_to_unit "refocus('var_id')";
            match Js.eval_to_string "getInput('var_id')" with
            | "" ->
                Js_of_ocaml.Dom.preventDefault event;
                Js_of_ocaml.Dom_html.stopPropagation event;
                None
            | str ->
                Js.eval_to_unit "setInput('var_id', '')";
                Js.eval_to_unit
                  ("refocus('instance" ^ string_of_int this_model.id ^ "')");
                Some (Edit (Create (Exp_var str))) )
        | ArrowLeft ->
            let prev_id =
              match Model.MapInt.find_opt (this_model.id - 1) model with
              | Some prev_model -> prev_model.id
              | None -> fst @@ Model.MapInt.max_binding model
            in
            Js.eval_to_unit @@ "refocus('instance" ^ string_of_int prev_id
            ^ "')";
            None
        | ArrowRight ->
            let next_id =
              match Model.MapInt.find_opt (this_model.id + 1) model with
              | Some next_model -> next_model.id
              | None -> fst @@ Model.MapInt.min_binding model
            in
            Js.eval_to_unit @@ "refocus('instance" ^ string_of_int next_id
            ^ "')";
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
