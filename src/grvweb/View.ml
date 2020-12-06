module Dom = Js_of_ocaml.Dom
module Dom_html = Js_of_ocaml.Dom_html
module Node = Virtual_dom.Vdom.Node
module Attr = Virtual_dom.Vdom.Attr
module Event = Virtual_dom.Vdom.Event

(* Attrs *)

let clicks_to ({ inject; editor } : Gui.context) (cursor : Cursor.t) : Attr.t =
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

let hole_node (ctx : Gui.context) (parent : Cursor.t) : Node.t =
  maybe_cursor_node ctx.editor parent
    (Node.span [ Attr.class_ "hole"; clicks_to ctx parent ] [ chars "□" ])

let conflict_node (ctx : Gui.context) (parent : Cursor.t) (nodes : Node.t list)
    : Node.t =
  maybe_cursor_node ctx.editor parent
    (Node.span
       [ Attr.class_ "conflict"; clicks_to ctx parent ]
       ([ errs "{ " ] @ Util.List.intersperse (errs " | ") nodes @ [ errs " }" ]))

let constructor_node (ctx : Gui.context) (parent : Cursor.t) (vertex : Vertex.t)
    (child_nodes_map : Node.t list Tree.IndexMap.t) : Node.t =
  let maybe_id_node =
    if ctx.editor.show_ids then
      [ Node.create "sub" [] [ Node.text (Uuid.Id.show vertex.id) ] ]
    else []
  in
  let nodes =
    [
      Node.span [ clicks_to ctx parent ]
        (Lang.show chars chars
           (fun index ->
             match Tree.IndexMap.find_opt index child_nodes_map with
             | None | Some [] -> hole_node ctx { vertex; index }
             | Some [ child_node ] -> child_node
             | Some child_nodes ->
                 conflict_node ctx { vertex; index } child_nodes)
           vertex.value);
    ]
  in
  Node.span [ Attr.class_ "vertex" ] (maybe_id_node @ nodes)

(* Views *)

let rec view_tree_constructor ?(at_top : bool = false)
    ?(with_parens : bool = true) (ctx : Gui.context) (parent_cursor : Cursor.t)
    (tree : Tree.t) : Node.t =
  maybe_cursor_node ctx.editor parent_cursor
    (view_tree ~at_top ~with_parens ctx (Some parent_cursor) tree)

and view_tree ?(at_top : bool = false) ?(with_parens : bool = true)
    (ctx : Gui.context) (parent : Cursor.t option) (tree : Tree.t) : Node.t =
  let parent_cursor = Option.value ~default:Cursor.root parent in
  let node =
    match tree with
    | Ref v -> ref_node ctx.editor parent_cursor v.id
    | Con (v, _) when v.id = Uuid.Id.read "0" && not at_top ->
        ref_node ctx.editor Cursor.root (Uuid.Id.read "0")
    | Con (vertex, subtrees_map) -> (
        let child_nodes_map =
          Tree.IndexMap.mapi
            (fun index subtrees ->
              List.map
                (view_tree_constructor ~at_top
                   ~with_parens:
                     (not (vertex = Vertex.root || vertex.value = Exp_lam))
                   ctx { vertex; index })
                subtrees)
            subtrees_map
        in
        let node = constructor_node ctx parent_cursor vertex child_nodes_map in
        match vertex.value with
        | Exp_app | Exp_plus | Exp_times ->
            if with_parens then parenthesize node else node
        | _ -> node )
  in
  maybe_cursor_node ctx.editor parent_cursor node

let view_editor (model : Model.t) (ctx : Gui.context)
    (tabindexes : int Uuid.Map.t) : Node.t =
  (* let roots = Graph.roots editor.graph in
     assert (roots.root = Curso r.root.vertex); *)
  let id : string = Uuid.Id.show ctx.editor.id in

  let tree_ctx : Tree.context = Tree.context ctx.editor.graph in
  let r, d, mp, sc = Tree.decompose tree_ctx in

  Graphviz.draw ctx.editor;
  Node.div
    [
      Attr.id ("editor" ^ id);
      Attr.class_ "editor";
      Attr.create "tabindex"
        (Int.to_string (Uuid.Map.find ctx.editor.id tabindexes));
      Attr.on_keydown
        (Key.dispatch ~inject:ctx.inject model ctx.editor tabindexes);
    ]
    [
      (* BEGIN MAIN EDIT VIEW *)
      view_tree ~at_top:true ~with_parens:false ctx None r;
      (* END MAIN EDIT VIEW *)
      Gui.break;
      Gui.panel ~label:"Cursor"
        [ chars (Format.asprintf "%a@." Cursor.pp ctx.editor.cursor) ];
      Gui.panel ~label:"Graph"
        [
          Node.div [ Attr.id ("graph" ^ id) ] [ Node.span [] [] ];
          Gui.break;
          Node.div []
            [
              Gui.button "Drop Edge" ctx tabindexes ~on_click:(fun () ->
                  match Js.prompt "edge_id" with
                  | "" -> None
                  | str -> Some (Edit (DropEdge (Uuid.Id.read str))));
              Gui.button "Show Source" ctx tabindexes ~on_click:(fun () ->
                  Printf.printf "%s\n"
                    (Graphviz.draw_graph ctx.editor.graph ctx.editor.cursor);
                  None);
              Gui.button "Toggle IDs" ctx tabindexes ~on_click:(fun () ->
                  Some (Env (ToggleIds ctx.editor.id)));
            ];
        ];
      Node.div
        [ Attr.class_ "selectors" ]
        [
          Gui.select_panel ~label:"Actions" ~multi:true ("actions" ^ id)
            (Graph_action.Set.elements ctx.editor.actions)
            (fun graph_action ->
              [ chars (Format.asprintf "%a" Graph_action.pp graph_action) ])
            [];
          Gui.select_panel ~label:"Send to Editors" ~multi:true
            ~classes:[ "Editors" ] ("editors" ^ id)
            (List.rev_map fst (Uuid.Map.bindings model.editors))
            (fun editor_id -> [ Node.text (Uuid.Id.show editor_id) ])
            [
              Gui.button "Send (ctrl-s)" ctx tabindexes ~on_click:(fun () ->
                  Gui.send model ctx.editor);
              Gui.button "All" ctx tabindexes ~on_click:(fun () ->
                  Js.fill_selection ("editors" ^ id);
                  None);
              Gui.button "None" ctx tabindexes ~on_click:(fun () ->
                  Js.clear_selection ("editors" ^ id);
                  None);
            ];
          (* TODO: fix buggy cursor inside Multiparented box *)
          Gui.select_panel ~label:"Multiparented" ~multi:false
            ("multiparent" ^ id) mp
            (fun tree ->
              let node =
                view_tree ~at_top:true ~with_parens:false ctx None tree
              in
              let vertex =
                match tree with
                | Ref vertex -> (
                    match Graph.vertex ctx.editor.graph vertex.id with
                    | None -> failwith __LOC__
                    | Some v -> v )
                | Con (vertex, _) -> vertex
              in
              let parent_nodes : Node.t list =
                Graph.parent_vertexes ctx.editor.graph vertex
                |> Vertex.Set.elements
                |> List.map
                     (Tree.reachable
                        (Graph.live_edges ctx.editor.graph)
                        (Graph.multiparented ctx.editor.graph))
                |> List.map fst
                |> List.map (view_tree ~at_top:true ~with_parens:false ctx None)
              in

              node :: parent_nodes)
            [];
          Gui.select_panel ~label:"Deleted" ~multi:false ("deleted" ^ id) d
            (fun tree ->
              [ view_tree ~at_top:true ~with_parens:false ctx None tree ])
            [
              ( Js.set_input ("restore" ^ id) "";
                Gui.panel
                  [
                    Gui.button "Restore" ctx tabindexes ~on_click:(fun () ->
                        Gui.restore ctx.editor tree_ctx.deleted
                          (Js.get_input ("restore" ^ id)));
                    Gui.text_input ("restore" ^ id) ctx tabindexes
                      ~on_change:(fun str ->
                        Gui.restore ctx.editor tree_ctx.deleted str);
                  ] );
            ];
          Gui.select_panel ~label:"Simple Cycles" ~multi:false ("cycles" ^ id)
            sc
            (fun tree -> [ view_tree ~with_parens:false ctx None tree ])
            [];
        ];
      Gui.panel ~label:"Patterns and Expressions"
        [
          Gui.sorted_button "Pat (p)" Lang.Sort.Pat ctx tabindexes
            ~on_click:(fun () ->
              match Js.prompt "pat_id" with
              | "" -> None
              | str ->
                  Js.focus ("editor" ^ id);
                  Some (Edit (Create (Pat_var str))));
          Gui.sorted_button "Var (v)" Lang.Sort.Exp ctx tabindexes
            ~on_click:(fun () ->
              match Js.prompt "var_id" with
              | "" -> None
              | str ->
                  Js.focus ("editor" ^ id);
                  Some (Edit (Create (Exp_var str))));
          Gui.sorted_button "Num (n)" Lang.Sort.Exp ctx tabindexes
            ~on_click:(fun () ->
              match Js.prompt "num_id" with
              | "" -> None
              | str ->
                  Js.focus ("editor" ^ id);
                  Some (Edit (Create (Exp_num (int_of_string str)))));
          Gui.sorted_button "Lam (\\)" Lang.Sort.Exp ctx tabindexes
            ~on_click:(fun () ->
              Js.focus ("editor" ^ id);
              Some (Edit (Create Exp_lam)));
          Gui.sorted_button "App (space)" Lang.Sort.Exp ctx tabindexes
            ~on_click:(fun () ->
              Js.focus ("editor" ^ id);
              Some (Edit (Create Exp_app)));
          Gui.sorted_button "Plus (+)" Lang.Sort.Exp ctx tabindexes
            ~on_click:(fun () ->
              Js.focus ("editor" ^ id);
              Some (Edit (Create Exp_plus)));
          Gui.sorted_button "Times (*)" Lang.Sort.Exp ctx tabindexes
            ~on_click:(fun () ->
              Js.focus ("editor" ^ id);
              Some (Edit (Create Exp_times)));
        ];
      Gui.panel ~label:"Types"
        [
          Gui.sorted_button "Num (N)" Lang.Sort.Typ ctx tabindexes
            ~on_click:(fun () -> Some (Edit (Create Typ_num)));
          Gui.sorted_button "Arrow (>)" Lang.Sort.Typ ctx tabindexes
            ~on_click:(fun () -> Some (Edit (Create Typ_arrow)));
        ];
      Gui.panel ~label:"Cursor"
        [
          Gui.button "Delete (delete)" ctx tabindexes ~on_click:(fun () ->
              Js.clear_selection ("deleted" ^ id);
              Some (Edit Destroy));
          Gui.button "Up (↑)" ctx tabindexes ~on_click:(fun () ->
              Some (Move Up));
          Gui.button "Down (↓)" ctx tabindexes ~on_click:(fun () ->
              Some (Move Down));
          Gui.button "Left (←)" ctx tabindexes ~on_click:(fun () ->
              Some (Move Left));
          Gui.button "Right (→)" ctx tabindexes ~on_click:(fun () ->
              Some (Move Right));
          Gui.button "Teleport" ctx tabindexes ~on_click:(fun () ->
              (* TODO: move Teleport logic into Gui.ml helper *)
              match Js.prompt "edge_id or vertex_id" with
              | "" -> None
              | str -> (
                  let teleport_id = Uuid.Id.read str in
                  Js.focus ("editor" ^ id);
                  match
                    Edge.Set.find_first_opt
                      (fun edge -> edge.id = teleport_id)
                      (Graph.edges ctx.editor.graph)
                  with
                  | Some edge -> Some (Move (Select edge.value.source))
                  | None -> (
                      match
                        Vertex.Set.find_first_opt
                          (fun vertex -> vertex.id = teleport_id)
                          (Graph.vertexes ctx.editor.graph)
                      with
                      | None -> None
                      | Some vertex -> (
                          match
                            Edge.Set.choose_opt
                              (Graph.parent_edges ctx.editor.graph vertex)
                          with
                          | Some edge -> Some (Move (Select edge.value.source))
                          | None -> None ) ) ));
        ];
      Gui.panel ~label:"Environment"
        [
          Gui.button "Record" ctx tabindexes
            ~on_click:(fun () -> Some (Env Record))
            ~disabled:(Option.is_some model.actions);
          Gui.button "Report" ctx tabindexes
            ~on_click:(fun () -> Some (Env Report))
            ~disabled:(Option.is_none model.actions);
          Gui.button "Stop" ctx tabindexes
            ~on_click:(fun () -> Some (Env Stop))
            ~disabled:(Option.is_none model.actions);
          Gui.button "Replay" ctx tabindexes ~on_click:(fun () ->
              Some (Env (Replay (Js.prompt "Replay Recording"))));
          Gui.button "Dump" ctx tabindexes ~on_click:(fun () -> Some (Env Dump));
          Gui.button "Load" ctx tabindexes ~on_click:(fun () ->
              Some (Env (Load (Js.prompt "Load a Dump"))));
        ];
      Gui.panel ~label:"Editor"
        [
          Gui.button "Clone" ctx tabindexes ~on_click:(fun () ->
              Some (Env (Clone ctx.editor.id)));
          Gui.button "Drop" ctx tabindexes
            ~on_click:(fun () -> Some (Env (Drop ctx.editor.id)))
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
    (List.map
       (fun editor -> view_editor model (Gui.context inject editor) tabindexes)
       editors)
