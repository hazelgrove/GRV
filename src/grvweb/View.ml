module Dom = Js_of_ocaml.Dom
module Dom_html = Js_of_ocaml.Dom_html
module Node = Virtual_dom.Vdom.Node
module Attr = Virtual_dom.Vdom.Attr
module Event = Virtual_dom.Vdom.Event

(* Attrs *)

let clicks_to (cursor : Cursor.t) (inject : Action.t -> Event.t)
    (editor : Editor.t) : Attr.t =
  Attr.on_click (fun event ->
      Dom.preventDefault event;
      Dom_html.stopPropagation event;
      inject { Action.editor_id = editor.id; action = Move (Select cursor) })

(* Nodes *)

let chars (str : string) : Node.t =
  Node.span [ Attr.class_ "chars" ] [ Node.text str ]

let errs (str : string) : Node.t =
  Node.span [ Attr.class_ "errs" ] [ Node.text str ]

let parenthesize (node : Node.t) : Node.t =
  Node.span [] [ chars "("; node; chars ")" ]

let cursor_node (node : Node.t) : Node.t =
  Node.span [ Attr.class_ "cursor" ] [ node ]

let maybe_cursor_node (editor : Editor.t) (parent_cursor : Cursor.t)
    (node : Node.t) : Node.t =
  if parent_cursor = editor.cursor then cursor_node node else node

let ref_node (editor : Editor.t) (parent_cursor : Cursor.t) (id : Uuid.Id.t) :
    Node.t =
  maybe_cursor_node editor parent_cursor
    (Node.span [ Attr.class_ "vertex" ] [ Node.text ("#" ^ Uuid.Id.show id) ])

let hole_node (inject : Action.t -> Event.t) (editor : Editor.t)
    (parent_cursor : Cursor.t) : Node.t =
  maybe_cursor_node editor parent_cursor
    (Node.span
       [ Attr.class_ "hole"; clicks_to parent_cursor inject editor ]
       [ chars "□" ])

let conflict_node (inject : Action.t -> Event.t) (editor : Editor.t)
    (parent_cursor : Cursor.t) (nodes : Node.t list) : Node.t =
  maybe_cursor_node editor parent_cursor
    (Node.span
       [ Attr.class_ "conflict"; clicks_to parent_cursor inject editor ]
       ([ errs "{ " ] @ Util.List.intersperse (errs " | ") nodes @ [ errs " }" ]))

let constructor_node (inject : Action.t -> Event.t) (editor : Editor.t)
    (parent : Cursor.t) (vertex : Vertex.t)
    (child_nodes_map : Node.t list Tree.IndexMap.t) : Node.t =
  let maybe_id_node =
    if editor.show_ids then
      [ Node.create "sub" [] [ Node.text (Uuid.Id.show vertex.id) ] ]
    else []
  in
  let nodes =
    [
      Node.span
        [ clicks_to parent inject editor ]
        (Lang.show chars chars
           (fun index ->
             match Tree.IndexMap.find_opt index child_nodes_map with
             | None | Some [] -> hole_node inject editor { vertex; index }
             | Some [ child_node ] -> child_node
             | Some child_nodes ->
                 conflict_node inject editor { vertex; index } child_nodes)
           vertex.value);
    ]
  in
  Node.span [ Attr.class_ "vertex" ] (maybe_id_node @ nodes)

(* Views *)

let rec view_tree_constructor ?(at_top : bool = false)
    ?(with_parens : bool = true) (inject : Action.t -> Event.t)
    (editor : Editor.t) (parent_cursor : Cursor.t) (tree : Tree.t) : Node.t =
  maybe_cursor_node editor parent_cursor
    (view_tree ~at_top ~with_parens inject editor (Some parent_cursor) tree)

and view_tree ?(at_top : bool = false) ?(with_parens : bool = true)
    (inject : Action.t -> Event.t) (editor : Editor.t)
    (parent : Cursor.t option) (tree : Tree.t) : Node.t =
  let parent_cursor = Option.value ~default:Cursor.root parent in
  let node =
    match tree with
    | Ref id -> ref_node editor parent_cursor id
    | Con (v, _) when v.id = Uuid.Id.read "0" && not at_top ->
        ref_node editor Cursor.root (Uuid.Id.read "0")
    | Con (vertex, subtrees_map) -> (
        let child_nodes_map =
          Tree.IndexMap.mapi
            (fun index subtrees ->
              List.map
                (view_tree_constructor ~at_top
                   ~with_parens:
                     (not (vertex = Vertex.root || vertex.value = Exp_lam))
                   inject editor { vertex; index })
                subtrees)
            subtrees_map
        in
        let node =
          constructor_node inject editor parent_cursor vertex child_nodes_map
        in
        match vertex.value with
        | Exp_app | Exp_plus | Exp_times ->
            if with_parens then parenthesize node else node
        | _ -> node )
  in
  maybe_cursor_node editor parent_cursor node

let view_editor (model : Model.t) (inject : Action.t -> Event.t)
    (tabindexes : int Uuid.Map.t) (editor : Editor.t) : Node.t =
  let roots = Graph.roots editor.graph in
  assert (roots.root = Cursor.root.vertex);
  let id = Uuid.Id.show editor.id in

  let multiparent = Graph.multiparents editor.graph in
  let reachable_tree, multiparent_trees, deleted_trees, simple_cycle_trees =
    Tree.decompose editor.graph multiparent
  in

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
      view_tree ~at_top:true ~with_parens:false inject editor None
        reachable_tree;
      Gui.break;
      Gui.panel ~label:"Cursor"
        [ chars (Format.asprintf "%a@." Cursor.pp editor.cursor) ];
      Gui.panel ~label:"Graph"
        [
          Node.div [ Attr.id ("graph" ^ id) ] [ Node.span [] [] ];
          Gui.break;
          Node.div []
            [
              Gui.button "Drop Edge" inject editor tabindexes
                ~on_click:(fun () ->
                  match Js.prompt "edge_id" with
                  | "" -> None
                  | str -> Some (Edit (DropEdge (Uuid.Id.read str))));
              Gui.button "Show Source" inject editor tabindexes
                ~on_click:(fun () ->
                  Printf.printf "%s\n"
                    (Graphviz.draw_graph editor.graph editor.cursor);
                  None);
              Gui.button "Toggle IDs" inject editor tabindexes
                ~on_click:(fun () -> Some (Env (ToggleIds editor.id)));
            ];
        ];
      Node.div
        [ Attr.class_ "selectors" ]
        [
          Gui.select_panel ~label:"Actions" ~multi:true ("actions" ^ id)
            (Graph_action.Set.elements editor.actions)
            (fun graph_action ->
              [ chars (Format.asprintf "%a" Graph_action.pp graph_action) ])
            [];
          Gui.select_panel ~label:"Send to Editors" ~multi:true
            ~classes:[ "Editors" ] ("editors" ^ id)
            (List.rev_map fst (Uuid.Map.bindings model.editors))
            (fun editor_id -> [ Node.text (Uuid.Id.show editor_id) ])
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
          ( Format.printf "multiparent = [";
            List.iter
              (fun t -> Format.printf "%s; " (Tree.show t))
              multiparent_trees;
            Format.printf "]%!";
            Gui.select_panel ~label:"Multiparented" ~multi:false
              ("multiparent" ^ id) multiparent_trees
              (fun tree ->
                let node =
                  view_tree ~with_parens:false inject editor None tree
                in
                let vertex =
                  match tree with
                  | Ref id -> (
                      match Graph.vertex editor.graph id with
                      | None ->
                          Format.printf "bad id2 %s%!" (Uuid.Id.show id);
                          failwith __LOC__
                      | Some v -> v )
                  | Con (vertex, _) -> vertex
                in
                let parent_trees =
                  List.map
                    (Tree.reachable editor.graph multiparent)
                    (Vertex.Set.elements
                       (Graph.parent_vertexes editor.graph vertex))
                in
                Format.printf "parent_trees = [";
                List.iter
                  (fun t -> Format.printf "%s; " (Tree.show t))
                  parent_trees;
                Format.printf "]%!";
                let parent_nodes =
                  List.map
                    (view_tree ~with_parens:false inject editor None)
                    parent_trees
                in
                node :: parent_nodes)
              [] );
          Gui.select_panel ~label:"Deleted" ~multi:false ("deleted" ^ id)
            deleted_trees
            (fun tree ->
              [ view_tree ~with_parens:false inject editor None tree ])
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
          Gui.select_panel ~label:"Simple Cycles" ~multi:false ("cycles" ^ id)
            simple_cycle_trees
            (fun tree ->
              [ view_tree ~with_parens:false inject editor None tree ])
            [];
        ];
      Gui.panel ~label:"Patterns and Expressions"
        [
          Gui.sorted_button "Pat (p)" Lang.Sort.Pat inject editor tabindexes
            ~on_click:(fun () ->
              match Js.prompt "pat_id" with
              | "" -> None
              | str ->
                  Js.focus ("editor" ^ id);
                  Some (Edit (Create (Pat_var str))));
          Gui.sorted_button "Var (v)" Lang.Sort.Exp inject editor tabindexes
            ~on_click:(fun () ->
              match Js.prompt "var_id" with
              | "" -> None
              | str ->
                  Js.focus ("editor" ^ id);
                  Some (Edit (Create (Exp_var str))));
          Gui.sorted_button "Num (n)" Lang.Sort.Exp inject editor tabindexes
            ~on_click:(fun () ->
              match Js.prompt "num_id" with
              | "" -> None
              | str ->
                  Js.focus ("editor" ^ id);
                  Some (Edit (Create (Exp_num (int_of_string str)))));
          Gui.sorted_button "Lam (\\)" Lang.Sort.Exp inject editor tabindexes
            ~on_click:(fun () ->
              Js.focus ("editor" ^ id);
              Some (Edit (Create Exp_lam)));
          Gui.sorted_button "App (space)" Lang.Sort.Exp inject editor tabindexes
            ~on_click:(fun () ->
              Js.focus ("editor" ^ id);
              Some (Edit (Create Exp_app)));
          Gui.sorted_button "Plus (+)" Lang.Sort.Exp inject editor tabindexes
            ~on_click:(fun () ->
              Js.focus ("editor" ^ id);
              Some (Edit (Create Exp_plus)));
          Gui.sorted_button "Times (*)" Lang.Sort.Exp inject editor tabindexes
            ~on_click:(fun () ->
              Js.focus ("editor" ^ id);
              Some (Edit (Create Exp_times)));
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
          Gui.button "Up (↑)" inject editor tabindexes ~on_click:(fun () ->
              Some (Move Up));
          Gui.button "Down (↓)" inject editor tabindexes ~on_click:(fun () ->
              Some (Move Down));
          Gui.button "Left (←)" inject editor tabindexes ~on_click:(fun () ->
              Some (Move Left));
          Gui.button "Right (→)" inject editor tabindexes ~on_click:(fun () ->
              Some (Move Right));
          Gui.button "Teleport" inject editor tabindexes ~on_click:(fun () ->
              (* TODO: move Teleport logic into Gui.ml helper *)
              match Js.prompt "edge_id or vertex_id" with
              | "" -> None
              | str -> (
                  let teleport_id = Uuid.Id.read str in
                  Js.focus ("editor" ^ id);
                  match
                    Edge.Set.find_first_opt
                      (fun edge -> edge.id = teleport_id)
                      (Graph.edges editor.graph)
                  with
                  | Some edge -> Some (Move (Select edge.value.source))
                  | None -> (
                      match
                        Vertex.Set.find_first_opt
                          (fun vertex -> vertex.id = teleport_id)
                          (Graph.vertexes editor.graph)
                      with
                      | None -> None
                      | Some vertex -> (
                          match
                            Edge.Set.choose_opt
                              (Graph.parents editor.graph vertex)
                          with
                          | Some edge -> Some (Move (Select edge.value.source))
                          | None -> None ) ) ));
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
