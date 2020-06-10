module Dom = Js_of_ocaml.Dom
module Dom_html = Js_of_ocaml.Dom_html
module Vdom = Virtual_dom.Vdom
module W = Widget

let clickable (inject : Action.t -> Vdom.Event.t) (editor : Editor.t)
    (cursor : Cursor.t) : Vdom.Attr.t =
  Vdom.Attr.on_click (fun event ->
      Dom.preventDefault event;
      Dom_html.stopPropagation event;
      inject { Action.editor_id = editor.id; action = Move (Select cursor) })

let rec view_cursor (inject : Action.t -> Vdom.Event.t) (editor : Editor.t)
    (seen : Vertex.Set.t ref) (cursor : Cursor.t) : Vdom.Node.t =
  let open Vdom.Node in
  let open Vdom.Attr in
  let node =
    let view_vertex' : Vertex.t -> Cursor.t option -> Vdom.Node.t =
      view_vertex inject editor seen
    in
    match Edge.Set.elements (Graph.children editor.graph cursor) with
    | [] ->
        span [ class_ "hole"; clickable inject editor cursor ] [ W.chars "_" ]
    | [ edge ] -> view_vertex' (Edge.target edge) (Some cursor)
    | edges ->
        let nodes =
          List.map
            (fun (edge : Edge.t) ->
              view_vertex' (Edge.target edge) (Some cursor))
            edges
        in
        span
          [ class_ "conflict"; clickable inject editor cursor ]
          ( [ W.errs "{" ]
          @ Util.List.intersperse (W.errs "|") nodes
          @ [ W.errs "}" ] )
  in
  if editor.cursor = cursor then span [ class_ "cursor" ] [ node ] else node

and view_vertex (inject : Action.t -> Vdom.Event.t) (editor : Editor.t)
    (seen : Vertex.Set.t ref) (vertex : Vertex.t) (parent : Cursor.t option) :
    Vdom.Node.t =
  let open Vdom.Node in
  let open Vdom.Attr in
  if Vertex.Set.mem vertex !seen then
    span [ class_ "vertex" ] [ text @@ "#" ^ Uuid.Id.show vertex.id ]
  else (
    seen := Vertex.Set.add vertex !seen;
    let node =
      let view_cursor' (index : Lang.Index.t) : Vdom.Node.t =
        view_cursor inject editor seen { vertex; index }
      in
      let attr =
        match parent with None -> [] | Some p -> [ clickable inject editor p ]
      in
      span attr (Lang.show W.chars W.chars view_cursor' vertex.value)
    in
    span [ class_ "vertex" ]
      [ Vdom.Node.create "sub" [] [ text @@ Uuid.Id.show vertex.id ]; node ] )

let view_editor (inject : Action.t -> Vdom.Event.t) (model : Model.t)
    (editor : Editor.t) : Vdom.Node.t =
  let open Action in
  let open Vdom.Node in
  let open Vdom.Attr in
  let mk (w : Vdom.Node.t W.t) : Vdom.Node.t = w inject editor in
  let seen = ref Vertex.Set.empty in
  let main_code = view_cursor inject editor seen Cursor.root in
  let deleted_code =
    W.select ~multi:false ~default:false
      ("deleted" ^ Uuid.Id.show editor.id)
      "Deleted"
      (Vertex.Set.elements (Graph.deleted editor.graph))
      (fun (vertex : Vertex.t) -> view_vertex inject editor seen vertex None)
  in
  Graphviz.draw editor;
  div
    [
      id @@ "editor" ^ Uuid.Id.show editor.id;
      class_ "editor";
      create "tabindex" (Uuid.Id.show editor.id);
      on_keydown @@ Key.dispatch ~inject model editor;
    ]
    [
      main_code;
      br [];
      br [];
      div []
        [
          text "Patterns: ";
          mk
          @@ W.input_button "Pat (p)" "pat_id" Lang.Sort.Pat (fun str ->
                 Pat_var str);
        ];
      div []
        [
          text "Expressions: ";
          mk
          @@ W.input_button "Var (v)" "var_id" Lang.Sort.Exp (fun str ->
                 Exp_var str);
          mk
          @@ W.input_button "Num (n)" "num_id" Lang.Sort.Exp (fun str ->
                 Exp_num (int_of_string str));
          mk @@ W.create_button "Lam (\\)" Exp_lam Lang.Sort.Exp;
          mk @@ W.create_button "App (space)" Exp_app Lang.Sort.Exp;
          mk @@ W.create_button "Plus (+)" Exp_plus Lang.Sort.Exp;
        ];
      div []
        [
          text "Types: ";
          mk @@ W.create_button "Num (N)" Typ_num Lang.Sort.Typ;
          mk @@ W.create_button "Arrow (>)" Typ_arrow Lang.Sort.Typ;
        ];
      div []
        [
          mk
          @@ W.button "Delete (delete)" (fun () ->
                 Js.clear_selection ("deleted" ^ Uuid.Id.show editor.id);
                 Some (Edit Destroy));
        ];
      div []
        [
          mk @@ W.move_button "Up (↑)" Up;
          mk @@ W.move_button "Down (↓)" Down;
          mk @@ W.move_button "Left (←)" Left;
          mk @@ W.move_button "Right (→)" Right;
        ];
      div [ class_ "selector" ]
        [
          mk
          @@ W.select
               ("actions" ^ Uuid.Id.show editor.id)
               "Actions"
               (Graph_action.Set.elements editor.actions)
               (fun (item : Graph_action.t) ->
                 W.chars @@ Format.asprintf "%a" Graph_action.pp item);
          mk @@ W.button "Send (ctrl-s)" (fun () -> Key.send editor);
        ];
      div [ class_ "selector" ]
        [
          mk @@ deleted_code;
          (let btn : Vdom.Node.t W.t =
             W.button "Restore" (fun () ->
                 Key.restore editor
                   (Js.get_input ("restore" ^ Uuid.Id.show editor.id)))
           in
           let txt : Vdom.Node.t W.t =
             W.text_input
               ("restore" ^ Uuid.Id.show editor.id)
               (function "" -> None | str -> Key.restore editor str)
           in
           Js.set_input ("restore" ^ Uuid.Id.show editor.id) "";
           div [] [ btn inject editor; txt inject editor ]);
        ];
      h2 [] [ text "Cursor" ];
      W.chars @@ Format.asprintf "%a@." Cursor.pp editor.cursor;
      h2 [] [ text "Graph" ];
      div [ id ("graph" ^ Uuid.Id.show editor.id) ] [ span [] [] ];
    ]

let view ~(inject : Action.t -> Vdom.Event.t) (model : Model.t) : Vdom.Node.t =
  Vdom.Node.div []
    (List.map
       (fun (_, editor) -> view_editor inject model editor)
       (Uuid.Map.bindings model))
