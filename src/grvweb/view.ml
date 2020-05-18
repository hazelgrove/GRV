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

let actions : (Lang.Constructor.t * string * Dom_html.Keyboard_code.t) list =
  [
    (* Id_id *)
    (Exp_var, "Var", KeyV);
    (Exp_lam, "Lam (\\)", Backslash);
    (Exp_app, "App (space)", Space);
    (* TODO: (Exp_num, "Lam", Backslash); *)
    (* TODO: Require Shift+Equal (i.e., plus) *)
    (Exp_plus, "Plus (=)", Equal);
    (Typ_num, "Num", KeyN);
    (* TODO: Require Shift+Period (i.e. >) *)
    (Typ_arrow, "Arrow (>)", Period);
    (* TODO: Typ_num *)
  ]

let keymap : (Dom_html.Keyboard_code.t, Lang.Constructor.t) Hashtbl.t =
  Hashtbl.of_seq
    (List.to_seq
       (List.map (fun (constructor, _, code) -> (code, constructor)) actions))

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
  let key (event : Dom_html.keyboardEvent Js.t) : Action.app Option.t =
    Option.map
      (fun action -> Enqueue action)
      (let code = Dom_html.Keyboard_code.of_event event in
       match code with
       (* TODO: Delete key *)
       | ArrowUp -> Some (Move Out)
       | ArrowDown -> Some (Move In)
       | ArrowLeft -> Some (Move Left)
       | ArrowRight -> Some (Move Right)
       | _ -> (
           match Hashtbl.find_opt keymap code with
           | Some constructor -> Some (Edit (Create constructor))
           | None -> None ))
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
            match
              if Js.to_bool event##.altKey then (None : Action.app Option.t)
              else if Js.to_bool event##.ctrlKey then ctrl_key event
              else key event
            with
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
          (List.map
             (fun (constructor, name, _) ->
               action_button name (Enqueue (Edit (Create constructor))))
             actions);
        action_button "Delete" (Enqueue (Edit Delete));
        action_button "Send" Send;
        action_button "In" (Enqueue (Move In));
        action_button "Out" (Enqueue (Move Out));
        action_button "Left" (Enqueue (Move Left));
        action_button "Right" (Enqueue (Move Right));
        br [];
        br [];
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
