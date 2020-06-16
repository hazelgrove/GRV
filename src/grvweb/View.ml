module Dom = Js_of_ocaml.Dom
module Dom_html = Js_of_ocaml.Dom_html
module Node = Virtual_dom.Vdom.Node
module Attr = Virtual_dom.Vdom.Attr
module Event = Virtual_dom.Vdom.Event

(* Nodes *)

let chars (str : string) : Node.t =
  Node.span [ Attr.class_ "chars" ] [ Node.text str ]

let errs (str : string) : Node.t =
  Node.span [ Attr.class_ "errs" ] [ Node.text str ]

(* Attrs *)

let clicks_to (cursor : Cursor.t) (inject : Action.t -> Event.t)
    (editor : Editor.t) : Attr.t =
  Attr.on_click (fun event ->
      Dom.preventDefault event;
      Dom_html.stopPropagation event;
      inject { Action.editor_id = editor.id; action = Move (Select cursor) })

(* Components *)

let rec view_cursor (inject : Action.t -> Event.t) (editor : Editor.t)
    (roots : Vertex.Set.t) (first_call : bool) (cursor : Cursor.t) : Node.t =
  let node =
    let view_vertex' : Cursor.t option -> Vertex.t -> Node.t =
      view_vertex inject editor roots ~first_call
    in
    match Edge.Set.elements (Graph.cursor_children editor.graph cursor) with
    | [] ->
        Node.span
          [ Attr.class_ "hole"; clicks_to cursor inject editor ]
          [ chars "_" ]
    | [ edge ] -> view_vertex' (Some cursor) (Edge.target edge)
    | edges ->
        let nodes =
          List.map (view_vertex' (Some cursor)) (List.map Edge.target edges)
        in
        Node.span
          [ Attr.class_ "conflict"; clicks_to cursor inject editor ]
          ([ errs "{" ] @ Util.List.intersperse (errs "|") nodes @ [ errs "}" ])
  in
  if editor.cursor = cursor then Node.span [ Attr.class_ "cursor" ] [ node ]
  else node

and view_vertex (inject : Action.t -> Event.t) (editor : Editor.t)
    (roots : Vertex.Set.t) ?(first_call = true) (parent : Cursor.t option)
    (vertex : Vertex.t) : Node.t =
  if (not first_call) && Vertex.Set.mem vertex roots then
    Node.span [ Attr.class_ "vertex" ]
      [ Node.text ("#" ^ Uuid.Id.show vertex.id) ]
  else
    Node.span [ Attr.class_ "vertex" ]
      [
        Node.create "sub" [] [ Node.text (Uuid.Id.show vertex.id) ];
        Node.span
          ( match parent with
          | None -> []
          | Some p -> [ clicks_to p inject editor ] )
          (Lang.show chars chars
             (fun index ->
               view_cursor inject editor roots false { vertex; index })
             vertex.value);
      ]

let view_editor (model : Model.t) (inject : Action.t -> Event.t)
    (editor : Editor.t) : Node.t =
  let roots = Graph.roots editor.graph in
  let root_vertexes =
    Vertex.Set.add roots.root (Vertex.Set.union roots.multiparent roots.deleted)
  in
  assert (roots.root = Cursor.root.vertex);
  Graphviz.draw editor;
  Node.div
    [
      Attr.id ("editor" ^ Uuid.Id.show editor.id);
      Attr.class_ "editor";
      Attr.create "tabindex" (Uuid.Id.show editor.id);
      Attr.on_keydown (Key.dispatch ~inject model editor);
    ]
    [
      (* main code *)
      view_cursor inject editor root_vertexes false Cursor.root;
      Node.br [];
      Node.br [];
      Node.div []
        [
          Node.text "Patterns: ";
          Node.div []
            [
              Gui.sorted_button "Pat (p)" Lang.Sort.Pat inject editor
                ~on_click:(fun () ->
                  match Js.get_input "pat_id" with
                  | "" -> None
                  | str -> Some (Edit (Create (Pat_var str))));
              Gui.sorted_text_input "pat_id" Lang.Sort.Pat inject editor
                ~on_change:(fun str -> Some (Edit (Create (Pat_var str))));
            ];
        ];
      Node.div []
        [
          Node.text "Expressions: ";
          Node.div []
            [
              Gui.sorted_button "Var (v)" Lang.Sort.Exp inject editor
                ~on_click:(fun () ->
                  match Js.get_input "var_id" with
                  | "" -> None
                  | str -> Some (Edit (Create (Exp_var str))));
              Gui.sorted_text_input "var_id" Lang.Sort.Exp inject editor
                ~on_change:(fun str -> Some (Edit (Create (Exp_var str))));
            ];
          Node.div []
            [
              Gui.sorted_button "Num (n)" Lang.Sort.Exp inject editor
                ~on_click:(fun () ->
                  match Js.get_input "num_id" with
                  | "" -> None
                  | str -> Some (Edit (Create (Exp_num (int_of_string str)))));
              Gui.sorted_text_input "num_id" Lang.Sort.Exp inject editor
                ~on_change:(fun str ->
                  Some (Edit (Create (Exp_num (int_of_string str)))));
            ];
          Gui.sorted_button "Lam (\\)" Lang.Sort.Exp inject editor
            ~on_click:(fun () -> Some (Edit (Create Exp_lam)));
          Gui.sorted_button "App (space)" Lang.Sort.Exp inject editor
            ~on_click:(fun () -> Some (Edit (Create Exp_app)));
          Gui.sorted_button "Plus (+)" Lang.Sort.Exp inject editor
            ~on_click:(fun () -> Some (Edit (Create Exp_plus)));
        ];
      Node.div []
        [
          Node.text "Types: ";
          Gui.sorted_button "Num (N)" Lang.Sort.Typ inject editor
            ~on_click:(fun () -> Some (Edit (Create Typ_num)));
          Gui.sorted_button "Arrow (>)" Lang.Sort.Typ inject editor
            ~on_click:(fun () -> Some (Edit (Create Typ_arrow)));
        ];
      Node.div []
        [
          Gui.button "Delete (delete)" inject editor ~on_click:(fun () ->
              Js.clear_selection ("deleted" ^ Uuid.Id.show editor.id);
              Some (Edit Destroy));
        ];
      Node.div []
        [
          Gui.button "Up (↑)" inject editor ~on_click:(fun () ->
              Some (Move Up));
          Gui.button "Down (↓)" inject editor ~on_click:(fun () ->
              Some (Move Down));
          Gui.button "Left (←)" inject editor ~on_click:(fun () ->
              Some (Move Left));
          Gui.button "Right (→)" inject editor ~on_click:(fun () ->
              Some (Move Right));
        ];
      Node.div []
        [
          Node.text "Debug: ";
          Gui.button "Record" inject editor
            ~disabled:(Option.is_some model.actions) ~on_click:(fun () ->
              Some (Debug Record));
          Gui.button "Report" inject editor
            ~disabled:(Option.is_none model.actions) ~on_click:(fun () ->
              Some (Debug Report));
          Gui.button "Stop" inject editor
            ~disabled:(Option.is_none model.actions) ~on_click:(fun () ->
              Some (Debug Stop));
          Gui.button "Replay" inject editor ~on_click:(fun () ->
              Some (Debug (Replay (Js.prompt "Replay Recording"))));
        ];
      Node.div [ Attr.class_ "selector" ]
        [
          Gui.select "Actions"
            ("actions" ^ Uuid.Id.show editor.id)
            (Graph_action.Set.elements editor.actions)
            (fun graph_action ->
              chars (Format.asprintf "%a" Graph_action.pp graph_action))
            ~multi:true;
          Gui.button "Send (ctrl-s)" inject editor ~on_click:(fun () ->
              Gui.send editor);
        ];
      Node.div [ Attr.class_ "selector" ]
        [
          Gui.select "Deleted"
            ("deleted" ^ Uuid.Id.show editor.id)
            (Vertex.Set.elements roots.deleted)
            (fun vertex -> view_vertex inject editor root_vertexes None vertex)
            ~multi:false;
          ( Js.set_input ("restore" ^ Uuid.Id.show editor.id) "";
            Node.div []
              [
                Gui.button "Restore" inject editor ~on_click:(fun () ->
                    Gui.restore editor
                      (Js.get_input ("restore" ^ Uuid.Id.show editor.id)));
                Gui.text_input
                  ("restore" ^ Uuid.Id.show editor.id)
                  inject editor
                  ~on_change:(fun str -> Gui.restore editor str);
              ] );
        ];
      Node.br [];
      Node.div [ Attr.class_ "selector" ]
        [
          Gui.select "Multiparented"
            ("multiparent" ^ Uuid.Id.show editor.id)
            (Vertex.Set.elements roots.multiparent)
            (fun vertex -> view_vertex inject editor root_vertexes None vertex)
            ~multi:false;
        ];
      Node.h2 [] [ Node.text "Cursor" ];
      chars (Format.asprintf "%a@." Cursor.pp editor.cursor);
      Node.h2 [] [ Node.text "Graph" ];
      Node.div
        [ Attr.id ("graph" ^ Uuid.Id.show editor.id) ]
        [ Node.span [] [] ];
    ]

let view ~(inject : Action.t -> Event.t) (model : Model.t) : Node.t =
  Node.div []
    (List.map (view_editor model inject)
       (List.map snd (Uuid.Map.bindings model.editors)))
