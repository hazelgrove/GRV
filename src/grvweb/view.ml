module Dom_html = Js_of_ocaml.Dom_html
module Js = Js_of_ocaml.Js
module Vdom = Virtual_dom.Vdom

let chars (str : string) : Vdom.Node.t =
  Vdom.Node.span [ Vdom.Attr.class_ "chars" ] [ Vdom.Node.text str ]

let rec intersperse (delim : 'a) (xs : 'a list) : 'a list =
  match xs with
  | [] | [ _ ] -> xs
  | x :: xs' -> x :: delim :: intersperse delim xs'

let rec of_index (graph : Graph.t) (cursor : Cursor.t) (child : Cursor.t) :
    Vdom.Node.t =
  let open Vdom.Node in
  let open Vdom.Attr in
  let node =
    match Edge.Set.elements (Cache.children child graph.cache) with
    | [] -> span [ class_ "hole" ] [ chars "_" ]
    | [ edge ] -> of_vertex graph cursor (Edge.target edge)
    | edges ->
        let nodes =
          List.map (fun edge -> of_vertex graph cursor (Edge.target edge)) edges
        in
        span [ class_ "conflict" ] (intersperse (chars "|") nodes)
  in
  if cursor = child then span [ class_ "cursor" ] [ node ] else node

and of_vertex (graph : Graph.t) (cursor : Cursor.t) (vertex : Vertex.t) :
    Vdom.Node.t =
  let open Vdom.Node in
  let open Vdom.Attr in
  let node =
    span []
      ( match vertex.value with
      | Root_root -> failwith __LOC__
      | Id_id s -> [ chars s ]
      | Exp_var -> [ of_index graph cursor { vertex; index = Exp_var_id } ]
      | Exp_lam ->
          [
            chars "(\\";
            of_index graph cursor { vertex; index = Exp_lam_param };
            chars ":";
            of_index graph cursor { vertex; index = Exp_lam_param_type };
            chars "->";
            of_index graph cursor { vertex; index = Exp_lam_body };
            chars ")";
          ]
      | Exp_app ->
          [
            chars "(";
            of_index graph cursor { vertex; index = Exp_app_fun };
            chars " ";
            of_index graph cursor { vertex; index = Exp_app_arg };
            chars ")";
          ]
      | Exp_num n -> [ chars (Int.to_string n) ]
      | Exp_plus ->
          [
            of_index graph cursor { vertex; index = Exp_plus_left };
            chars "+";
            of_index graph cursor { vertex; index = Exp_plus_right };
          ]
      | Typ_num -> [ chars "Num" ]
      | Typ_arrow ->
          [
            of_index graph cursor { vertex; index = Typ_arrow_arg };
            chars "+";
            of_index graph cursor { vertex; index = Typ_arrow_result };
          ] )
  in
  span [ class_ "vertex" ]
    [ Vdom.Node.create "sub" [] [ text @@ Uuid.Id.show vertex.id ]; node ]

let view_instance (instance_id : int) ~(inject : Action.t -> Vdom.Event.t)
    (model : Model.Instance.t) : Vdom.Node.t =
  let open Action in
  let open Vdom.Node in
  let open Vdom.Attr in
  let ctrl_key (event : Dom_html.keyboardEvent Js.t) : Action.app Option.t =
    match Dom_html.Keyboard_code.of_event event with
    | KeyS -> Some Send
    | _ -> None
  in
  let shift_key (event : Dom_html.keyboardEvent Js.t) : Action.app Option.t =
    let key : string =
      Option.value
        (Option.map Js.to_string (Js.Optdef.to_option event##.key))
        ~default:""
    in
    Option.map (fun action -> Enqueue action)
    @@
    match key with
    | "+" -> Some (Edit (Create Exp_plus))
    | ">" -> Some (Edit (Create Typ_arrow))
    | _ -> None
  in
  let base_key (event : Dom_html.keyboardEvent Js.t) : Action.app Option.t =
    Option.map (fun action -> Enqueue action)
    @@
    match Dom_html.Keyboard_code.of_event event with
    | KeyN -> Some (Edit (Create Typ_num))
    | KeyV -> Some (Edit (Create Exp_var))
    | Space -> Some (Edit (Create Exp_app))
    | Backslash -> Some (Edit (Create Exp_lam))
    | Delete -> Some (Edit Delete)
    | ArrowUp -> Some (Move Out)
    | ArrowDown -> Some (Move In)
    | ArrowLeft -> Some (Move Left)
    | ArrowRight -> Some (Move Right)
    | _ -> None
  in
  let action_button (label : string) (action : Action.app) : Vdom.Node.t =
    button [ on_click (fun _ -> inject { instance_id; action }) ] [ text label ]
  in
  let result =
    div
      [
        class_ "instance";
        tabindex instance_id;
        on_keydown (fun event ->
            let handler =
              match
                Js.
                  ( to_bool event##.shiftKey,
                    to_bool event##.ctrlKey,
                    to_bool event##.altKey )
              with
              | false, false, false -> base_key
              | true, false, false -> shift_key
              | false, true, false -> ctrl_key
              | _, _, _ -> fun _ -> (None : Action.app Option.t)
            in
            match handler event with
            | Some action ->
                Js_of_ocaml.Dom.preventDefault event;
                inject { instance_id; action }
            | None -> Vdom.Event.Ignore);
      ]
      [
        of_index model.graph model.cursor Cursor.root;
        br [];
        br [];
        div []
          [
            action_button "Var (v)" (Enqueue (Edit (Create Exp_var)));
            action_button "Lam (\\)" (Enqueue (Edit (Create Exp_lam)));
            action_button "App (space)" (Enqueue (Edit (Create Exp_app)));
            action_button "Plus (+)" (Enqueue (Edit (Create Exp_plus)));
            action_button "Num (n)" (Enqueue (Edit (Create Typ_num)));
            action_button "Arrow (>)" (Enqueue (Edit (Create Typ_arrow)));
          ];
        div []
          [
            action_button "Delete (delete)" (Enqueue (Edit Delete));
            action_button "Send (ctrl-s)" Send;
            action_button "In (↓)" (Enqueue (Move In));
            action_button "Out (↑)" (Enqueue (Move Out));
            action_button "Left (←)" (Enqueue (Move Left));
            action_button "Right (→)" (Enqueue (Move Right));
          ];
        select
          [ create "size" "10"; bool_property "multiple" true; disabled ]
          (List.rev_map
             (fun action ->
               option [] [ text @@ Format.asprintf "%a" Graph_action.pp action ])
             model.actions);
        br [];
        br [];
        text "Cursor";
        br [];
        pre [] [ text @@ Format.asprintf "%a@." Cursor.pp model.cursor ];
        br [];
        text "Graph";
        br [];
        br [];
        div [ id @@ Printf.sprintf "graph%d" instance_id ] [ span [] [] ];
      ]
  in
  Viz.draw instance_id model.graph model.cursor;
  result

let view ~(inject : Action.t -> Vdom.Event.t) (model : Model.t) : Vdom.Node.t =
  Vdom.Node.div []
    (List.map
       (fun (i, m) -> view_instance i m ~inject)
       (Model.MapInt.bindings model))
