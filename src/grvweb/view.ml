module Dom_html = Js_of_ocaml.Dom_html
module Js = Js_of_ocaml.Js
module Vdom = Virtual_dom.Vdom

let chars (str : string) : Vdom.Node.t =
  Vdom.Node.span [ Vdom.Attr.class_ "chars" ] [ Vdom.Node.text str ]

let errs (str : string) : Vdom.Node.t =
  Vdom.Node.span [ Vdom.Attr.class_ "errs" ] [ Vdom.Node.text str ]

let rec intersperse (delim : 'a) (xs : 'a list) : 'a list =
  match xs with
  | [] | [ _ ] -> xs
  | x :: xs' -> x :: delim :: intersperse delim xs'

let rec of_index ~inject (model : Model.Instance.t) (child : Cursor.t) :
    Vdom.Node.t =
  let open Vdom.Node in
  let open Vdom.Attr in
  let node =
    let recur : Vertex.t -> Vdom.Node.t = of_vertex ~inject model in
    match Edge.Set.elements (Cache.children child model.graph.cache) with
    | [] -> span [ class_ "hole" ] [ chars "_" ]
    | [ edge ] -> recur @@ Edge.target edge
    | edges ->
        let nodes = List.map (fun edge -> recur @@ Edge.target edge) edges in
        span [ class_ "conflict" ]
          ([ errs "{" ] @ intersperse (errs "|") nodes @ [ errs "}" ])
  in
  if model.cursor = child then span [ class_ "cursor" ] [ node ] else node

and of_vertex ~inject (model : Model.Instance.t) (vertex : Vertex.t) :
    Vdom.Node.t =
  let open Vdom.Node in
  let open Vdom.Attr in
  let node =
    let recur : Cursor.t -> Vdom.Node.t = of_index ~inject model in
    span
      [
        ( on_click @@ fun event ->
          Js_of_ocaml.Dom.preventDefault event;
          Js_of_ocaml.Dom_html.stopPropagation event;
          inject { Action.instance_id = model.id; action = Select vertex.id } );
      ]
      ( match vertex.value with
      | Root_root -> failwith __LOC__
      | Exp_var s -> [ chars s ]
      | Exp_lam ->
          [
            chars "(\\";
            recur { vertex; index = Exp_lam_param };
            chars ":";
            recur { vertex; index = Exp_lam_param_type };
            chars "->";
            recur { vertex; index = Exp_lam_body };
            chars ")";
          ]
      | Exp_app ->
          [
            chars "(";
            recur { vertex; index = Exp_app_fun };
            chars " ";
            recur { vertex; index = Exp_app_arg };
            chars ")";
          ]
      | Exp_num n -> [ chars (Int.to_string n) ]
      | Exp_plus ->
          [
            recur { vertex; index = Exp_plus_left };
            chars "+";
            recur { vertex; index = Exp_plus_right };
          ]
      | Pat_var s -> [ chars s ]
      | Typ_num -> [ chars "Num" ]
      | Typ_arrow ->
          [
            recur { vertex; index = Typ_arrow_arg };
            chars "->";
            recur { vertex; index = Typ_arrow_result };
          ] )
  in
  span [ class_ "vertex" ]
    [ Vdom.Node.create "sub" [] [ text @@ Uuid.Id.show vertex.id ]; node ]

let view_instance ~(inject : Action.t -> Vdom.Event.t)
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
  let button_ ?(disabled : bool = false) (label : string) (action : Action.app)
      : Vdom.Node.t =
    let attrs =
      [ on_click (fun _ -> inject { instance_id = model.id; action }) ]
    in
    button
      (attrs @ if disabled then [ Vdom.Attr.disabled ] else [])
      [ text label ]
  in
  let create_button (label : string) (ctor : Lang.Constructor.t)
      (sort : Lang.Sort.t) : Vdom.Node.t =
    let disabled = not (Lang.Index.child_sort model.cursor.index = sort) in
    button_ ~disabled label @@ Enqueue (Edit (Create ctor))
  in
  let move_button (label : string) (dir : Action.direction) : Vdom.Node.t =
    let action = Enqueue (Move dir) in
    button
      [ on_click (fun _ -> inject { instance_id = model.id; action }) ]
      [ text label ]
  in
  let result =
    div
      [
        class_ "instance";
        tabindex model.id;
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
                inject { instance_id = model.id; action }
            | None -> Vdom.Event.Ignore);
      ]
      [
        of_index ~inject model Cursor.root;
        br [];
        br [];
        div []
          [
            create_button "Pat (p)" (Pat_var "P") Lang.Sort.Pat;
            create_button "Var (v)" (Exp_var "X") Lang.Sort.Exp;
            create_button "Lam (\\)" Exp_lam Lang.Sort.Exp;
            create_button "App (space)" Exp_app Lang.Sort.Exp;
            create_button "Plus (+)" Exp_plus Lang.Sort.Exp;
            create_button "Num (n)" Typ_num Lang.Sort.Typ;
            create_button "Arrow (>)" Typ_arrow Lang.Sort.Typ;
          ];
        div []
          [
            button_ "Delete (delete)" (Enqueue (Edit Destroy));
            button_ "Send (ctrl-s)" Send;
            move_button "In (↓)" In;
            move_button "Out (↑)" Out;
            move_button "Left (←)" Left;
            move_button "Right (→)" Right;
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
        div [ id @@ Printf.sprintf "graph%d" model.id ] [ span [] [] ];
      ]
  in
  Viz.draw model;
  result

let view ~(inject : Action.t -> Vdom.Event.t) (model : Model.t) : Vdom.Node.t =
  Vdom.Node.div []
    (List.map
       (fun (_, m) -> view_instance m ~inject)
       (Model.MapInt.bindings model))
