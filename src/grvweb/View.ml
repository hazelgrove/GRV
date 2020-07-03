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
    (tabindexes : int Uuid.Map.t) (editor : Editor.t) : Node.t =
  let roots = Graph.roots editor.graph in
  let root_vertexes =
    Vertex.Set.add roots.root (Vertex.Set.union roots.multiparent roots.deleted)
  in
  assert (roots.root = Cursor.root.vertex);
  let id = Uuid.Id.show editor.id in
  Graphviz.draw editor;
  Node.div
    [
      Attr.id ("editor" ^ id);
      Attr.class_ "editor";
      Attr.create "tabindex"
        (Int.to_string (Uuid.Map.find editor.id tabindexes));
      Attr.on_keydown (Key.dispatch ~inject model editor tabindexes);
    ]
    [
      (* main code *)
      view_cursor inject editor root_vertexes false Cursor.root;
      Gui.break;
      Gui.panel ~label:"Cursor"
        [ chars (Format.asprintf "%a@." Cursor.pp editor.cursor) ];
      Gui.panel ~label:"Graph"
        [
          Node.div [ Attr.id ("graph" ^ id) ] [ Node.span [] [] ];
          Gui.break;
          Node.div []
            [
              Gui.button "Show Source" inject editor tabindexes
                ~on_click:(fun () ->
                  Printf.printf "%s\n"
                    (Graphviz.draw_graph editor.graph editor.cursor);
                  None);
            ];
        ];
      Node.div
        [ Attr.class_ "selectors" ]
        [
          Gui.select_panel ~label:"Actions" ~multi:true ("actions" ^ id)
            (Graph_action.Set.elements editor.actions)
            (fun graph_action ->
              chars (Format.asprintf "%a" Graph_action.pp graph_action))
            [];
          Gui.select_panel ~label:"Send to Editors" ~multi:true
            ~classes:[ "Editors" ] ("editors" ^ id)
            (List.rev_map fst (Uuid.Map.bindings model.editors))
            (fun editor_id -> Node.text (Uuid.Id.show editor_id))
            [
              Gui.button "Send (ctrl-s)" inject editor tabindexes
                ~on_click:(fun () -> Gui.send model editor);
              Gui.button "All" inject editor tabindexes ~on_click:(fun () ->
                  Js.fill_selection ("editors" ^ id);
                  None);
              Gui.button "None" inject editor tabindexes ~on_click:(fun () ->
                  Js.clear_selection ("editors" ^ id);
                  None);
            ];
          Gui.select_panel ~label:"Multiparented" ~multi:false
            ("multiparent" ^ id)
            (Vertex.Set.elements roots.multiparent)
            (fun vertex -> view_vertex inject editor root_vertexes None vertex)
            [];
          Gui.select_panel ~label:"Deleted" ~multi:false ("deleted" ^ id)
            (Vertex.Set.elements roots.deleted)
            (fun vertex -> view_vertex inject editor root_vertexes None vertex)
            [
              ( Js.set_input ("restore" ^ id) "";
                Gui.panel
                  [
                    Gui.button "Restore" inject editor tabindexes
                      ~on_click:(fun () ->
                        Gui.restore editor (Js.get_input ("restore" ^ id)));
                    Gui.text_input ("restore" ^ id) inject editor tabindexes
                      ~on_change:(fun str -> Gui.restore editor str);
                  ] );
            ];
        ];
      Gui.panel ~label:"Patterns"
        [
          Gui.sorted_button "Pat (p)" Lang.Sort.Pat inject editor tabindexes
            ~on_click:(fun () ->
              match Js.get_input ("pat_id" ^ id) with
              | "" -> None
              | str -> Some (Edit (Create (Pat_var str))));
          Gui.sorted_text_input ("pat_id" ^ id) Lang.Sort.Pat inject editor
            tabindexes ~on_change:(fun str ->
              Some (Edit (Create (Pat_var str))));
        ];
      Gui.panel ~label:"Expressions"
        [
          Gui.sorted_button "Var (v)" Lang.Sort.Exp inject editor tabindexes
            ~on_click:(fun () ->
              match Js.get_input ("var_id" ^ id) with
              | "" -> None
              | str -> Some (Edit (Create (Exp_var str))));
          Gui.sorted_text_input ("var_id" ^ id) Lang.Sort.Exp inject editor
            tabindexes ~on_change:(fun str ->
              Some (Edit (Create (Exp_var str))));
          Gui.break;
          Gui.sorted_button "Num (n)" Lang.Sort.Exp inject editor tabindexes
            ~on_click:(fun () ->
              match Js.get_input ("num_id" ^ id) with
              | "" -> None
              | str -> Some (Edit (Create (Exp_num (int_of_string str)))));
          Gui.sorted_text_input ("num_id" ^ id) Lang.Sort.Exp inject editor
            tabindexes ~on_change:(fun str ->
              Some (Edit (Create (Exp_num (int_of_string str)))));
          Gui.break;
          Gui.sorted_button "Lam (\\)" Lang.Sort.Exp inject editor tabindexes
            ~on_click:(fun () -> Some (Edit (Create Exp_lam)));
          Gui.sorted_button "App (space)" Lang.Sort.Exp inject editor tabindexes
            ~on_click:(fun () -> Some (Edit (Create Exp_app)));
          Gui.sorted_button "Plus (+)" Lang.Sort.Exp inject editor tabindexes
            ~on_click:(fun () -> Some (Edit (Create Exp_plus)));
        ];
      Gui.panel ~label:"Types"
        [
          Gui.sorted_button "Num (N)" Lang.Sort.Typ inject editor tabindexes
            ~on_click:(fun () -> Some (Edit (Create Typ_num)));
          Gui.sorted_button "Arrow (>)" Lang.Sort.Typ inject editor tabindexes
            ~on_click:(fun () -> Some (Edit (Create Typ_arrow)));
        ];
      Gui.panel ~label:"Cursor"
        [
          Gui.button "Delete (delete)" inject editor tabindexes
            ~on_click:(fun () ->
              Js.clear_selection ("deleted" ^ id);
              Some (Edit Destroy));
          Gui.break;
          Gui.button "Up (↑)" inject editor tabindexes ~on_click:(fun () ->
              Some (Move Up));
          Gui.button "Down (↓)" inject editor tabindexes ~on_click:(fun () ->
              Some (Move Down));
          Gui.button "Left (←)" inject editor tabindexes ~on_click:(fun () ->
              Some (Move Left));
          Gui.button "Right (→)" inject editor tabindexes ~on_click:(fun () ->
              Some (Move Right));
        ];
      Gui.panel ~label:"Environment"
        [
          Gui.button "Record" inject editor tabindexes
            ~on_click:(fun () -> Some (Env Record))
            ~disabled:(Option.is_some model.actions);
          Gui.button "Report" inject editor tabindexes
            ~on_click:(fun () -> Some (Env Report))
            ~disabled:(Option.is_none model.actions);
          Gui.button "Stop" inject editor tabindexes
            ~on_click:(fun () -> Some (Env Stop))
            ~disabled:(Option.is_none model.actions);
          Gui.button "Replay" inject editor tabindexes ~on_click:(fun () ->
              Some (Env (Replay (Js.prompt "Replay Recording"))));
          Gui.button "Dump" inject editor tabindexes ~on_click:(fun () ->
              Some (Env Dump));
          Gui.button "Load" inject editor tabindexes ~on_click:(fun () ->
              Some (Env (Load (Js.prompt "Load a Dump"))));
        ];
      Gui.panel ~label:"Editor"
        [
          Gui.button "Clone" inject editor tabindexes ~on_click:(fun () ->
              Some (Env (Clone editor.id)));
          Gui.button "Drop" inject editor tabindexes
            ~on_click:(fun () -> Some (Env (Drop editor.id)))
            ~disabled:(Uuid.Map.cardinal model.editors < 2);
        ];
    ]

let view ~(inject : Action.t -> Event.t) (model : Model.t) : Node.t =
  let editors = List.map snd (Uuid.Map.bindings model.editors) in
  let editor_ids = List.map (fun (editor : Editor.t) -> editor.id) editors in
  let indexes = List.init (List.length editors) (fun i -> i + 1) in
  let tabindexes = Uuid.Map.of_seq List.(to_seq (combine editor_ids indexes)) in
  Node.div
    [ Attr.create "tabindex" "-1" ]
    (List.map (view_editor model inject tabindexes) editors)
