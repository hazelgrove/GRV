module Dom = Js_of_ocaml.Dom
module Dom_html = Js_of_ocaml.Dom_html
module Vdom = Virtual_dom.Vdom
module W = Widget

(* Nodes *)

let chars (str : string) : Vdom.Node.t =
  Vdom.Node.span [ Vdom.Attr.class_ "chars" ] [ Vdom.Node.text str ]

let errs (str : string) : Vdom.Node.t =
  Vdom.Node.span [ Vdom.Attr.class_ "errs" ] [ Vdom.Node.text str ]

(* Attrs *)

let clicks_to (cursor : Cursor.t) (inject : Action.t -> Vdom.Event.t)
    (editor : Editor.t) : Vdom.Attr.t =
  Vdom.Attr.on_click (fun event ->
      Dom.preventDefault event;
      Dom_html.stopPropagation event;
      inject { Action.editor_id = editor.id; action = Move (Select cursor) })

(* Components *)

let rec view_cursor (inject : Action.t -> Vdom.Event.t) (editor : Editor.t)
    (roots : Vertex.Set.t) (first_call : bool) (cursor : Cursor.t) : Vdom.Node.t
    =
  let open Vdom.Node in
  let open Vdom.Attr in
  let node =
    let view_vertex' : Cursor.t option -> Vertex.t -> Vdom.Node.t =
      view_vertex inject editor roots ~first_call
    in
    match Edge.Set.elements (Graph.cursor_children editor.graph cursor) with
    | [] -> span [ class_ "hole"; clicks_to cursor inject editor ] [ chars "_" ]
    | [ edge ] -> view_vertex' (Some cursor) (Edge.target edge)
    | edges ->
        let nodes =
          List.map (view_vertex' (Some cursor)) (List.map Edge.target edges)
        in
        span
          [ class_ "conflict"; clicks_to cursor inject editor ]
          ([ errs "{" ] @ Util.List.intersperse (errs "|") nodes @ [ errs "}" ])
  in
  if editor.cursor = cursor then span [ class_ "cursor" ] [ node ] else node

and view_vertex (inject : Action.t -> Vdom.Event.t) (editor : Editor.t)
    (roots : Vertex.Set.t) ?(first_call = true) (parent : Cursor.t option)
    (vertex : Vertex.t) : Vdom.Node.t =
  let open Vdom.Node in
  let open Vdom.Attr in
  if (not first_call) && Vertex.Set.mem vertex roots then
    span [ class_ "vertex" ] [ text @@ "#" ^ Uuid.Id.show vertex.id ]
  else
    span [ class_ "vertex" ]
      [
        Vdom.Node.create "sub" [] [ text @@ Uuid.Id.show vertex.id ];
        span
          ( match parent with
          | None -> []
          | Some p -> [ clicks_to p inject editor ] )
          (Lang.show chars chars
             (fun index ->
               view_cursor inject editor roots false { vertex; index })
             vertex.value);
      ]

let view_editor (model : Model.t) (inject : Action.t -> Vdom.Event.t)
    (editor : Editor.t) : Vdom.Node.t =
  let open Action in
  let open Vdom.Node in
  let open Vdom.Attr in
  let mk (w : Vdom.Node.t W.t) : Vdom.Node.t = w inject editor in
  let roots = Graph.roots editor.graph in
  let root_vertexes =
    Vertex.Set.add roots.root (Vertex.Set.union roots.multiparent roots.deleted)
  in
  assert (roots.root = Cursor.root.vertex);
  let main_code = view_cursor inject editor root_vertexes true Cursor.root in
  let deleted_code =
    W.select ~multi:false ~default:false
      ("deleted" ^ Uuid.Id.show editor.id)
      "Deleted"
      (Vertex.Set.elements roots.deleted)
      (view_vertex inject editor root_vertexes None)
  in
  let multiparent_code =
    W.select ~multi:false ~default:false
      ("multiparent" ^ Uuid.Id.show editor.id)
      "multiparent"
      (Vertex.Set.elements roots.multiparent)
      (view_vertex inject editor root_vertexes None)
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
                 chars @@ Format.asprintf "%a" Graph_action.pp item);
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
      br [];
      div [ class_ "selector" ] [ mk multiparent_code ];
      h2 [] [ text "Cursor" ];
      chars @@ Format.asprintf "%a@." Cursor.pp editor.cursor;
      h2 [] [ text "Graph" ];
      div [ id ("graph" ^ Uuid.Id.show editor.id) ] [ span [] [] ];
    ]

let view ~(inject : Action.t -> Vdom.Event.t) (model : Model.t) : Vdom.Node.t =
  Vdom.Node.div []
    (List.map (view_editor model inject)
       (List.map snd (Uuid.Map.bindings model)))
