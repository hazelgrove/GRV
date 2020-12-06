module Vdom = Virtual_dom.Vdom
module Node = Virtual_dom.Vdom.Node
module Attr = Virtual_dom.Vdom.Attr

(* Vdom Node Construtors *)

let chars (str : string) : Node.t =
  Node.span [ Attr.class_ "chars" ] [ Node.text str ]

let errs (str : string) : Node.t =
  Node.span [ Attr.class_ "errs" ] [ Node.text str ]

let parenthesize (node : Node.t) : Node.t =
  Node.span [] [ chars "("; node; chars ")" ]

let cursor_node (node : Node.t) : Node.t =
  Node.span [ Attr.class_ "cursor" ] [ node ]

let maybe_cursor_node (editor : Editor.t) (parent : Cursor.t) (node : Node.t) :
    Node.t =
  if parent = editor.cursor then cursor_node node else node

let ref_node (editor : Editor.t) (parent : Cursor.t) (id : Uuid.Id.t) : Node.t =
  Node.span [ Attr.class_ "vertex" ] [ Node.text ("#" ^ Uuid.Id.show id) ]
  |> maybe_cursor_node editor parent

let hole_node (ctx : Gui.context) (parent : Cursor.t) : Node.t =
  Node.span [ Attr.class_ "hole"; Gui.clicks_to ctx parent ] [ chars "□" ]
  |> maybe_cursor_node ctx.editor parent

let conflict_node (ctx : Gui.context) (parent : Cursor.t) (nodes : Node.t list)
    : Node.t =
  Node.span
    [ Attr.class_ "conflict"; Gui.clicks_to ctx parent ]
    ([ errs "{ " ] @ Util.List.intersperse (errs " | ") nodes @ [ errs " }" ])
  |> maybe_cursor_node ctx.editor parent

let constructor_node (ctx : Gui.context) (parent : Cursor.t) (vertex : Vertex.t)
    (child_nodes_map : Node.t list Tree.IndexMap.t) : Node.t =
  let maybe_id_node =
    if ctx.editor.show_ids then
      [ Node.create "sub" [] [ Node.text (Uuid.Id.show vertex.id) ] ]
    else []
  in
  let nodes =
    [
      Node.span
        [ Gui.clicks_to ctx parent ]
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

(* Panels *)

let rec view_tree_constructor ?(at_top : bool = false)
    ?(with_parens : bool = true) (ctx : Gui.context) (parent : Cursor.t)
    (tree : Tree.t) : Node.t =
  view_tree ctx (Some parent) tree ~at_top ~with_parens
  |> maybe_cursor_node ctx.editor parent

and view_tree ?(at_top : bool = false) ?(with_parens : bool = true)
    (ctx : Gui.context) (parent_opt : Cursor.t option) (tree : Tree.t) : Node.t
    =
  let parent = Option.value parent_opt ~default:Cursor.root in
  ( match tree with
  | Ref v -> ref_node ctx.editor parent v.id
  | Con (v, _) when v.id = Uuid.Id.read "0" && not at_top ->
      ref_node ctx.editor Cursor.root (Uuid.Id.read "0")
  | Con (vertex, subtrees_map) -> (
      let child_nodes_map =
        Tree.IndexMap.mapi
          (fun index subtrees ->
            List.map
              (view_tree_constructor ctx { vertex; index } ~at_top
                 ~with_parens:
                   (not (vertex = Vertex.root || vertex.value = Exp_lam)))
              subtrees)
          subtrees_map
      in
      let node = constructor_node ctx parent vertex child_nodes_map in
      match vertex.value with
      | Exp_app | Exp_plus | Exp_times ->
          if with_parens then parenthesize node else node
      | _ -> node ) )
  |> maybe_cursor_node ctx.editor parent

let cursor_panel (ctx : Gui.context) : Node.t =
  Gui.panel ~label:"Cursor"
    [ chars (Format.asprintf "%a@." Cursor.pp ctx.editor.cursor) ]

let graph_panel (ctx : Gui.context) (id : string) (tabindexes : int Uuid.Map.t)
    : Node.t =
  Gui.panel ~label:"Graph"
    [
      Node.div [ Attr.id ("graph" ^ id) ] [ Node.span [] [] ];
      Gui.break;
      Node.div []
        [
          Gui.button ctx "Drop Edge" tabindexes ~on_click:(fun () ->
              match Js.prompt "edge_id" with
              | "" -> None
              | str -> Some (Edit (DropEdge (Uuid.Id.read str))));
          Gui.none_button ctx "Show Source" tabindexes ~on_click:(fun () ->
              Format.printf "%s\n%!"
                (Graphviz.draw_graph ctx.editor.graph ctx.editor.cursor));
          Gui.some_button ctx "Toggle IDs" tabindexes
            (Env (ToggleIds ctx.editor.id));
        ];
    ]

let actions_panel (ctx : Gui.context) (id : string) : Node.t =
  Gui.select_panel ~label:"Actions" ~multi:true ("actions" ^ id)
    (Graph_action.Set.elements ctx.editor.actions)
    (fun graph_action ->
      [ chars (Format.asprintf "%a" Graph_action.pp graph_action) ])
    []

let send_actions_panel (model : Model.t) (ctx : Gui.context) (id : string)
    (tabindexes : int Uuid.Map.t) : Node.t =
  Gui.select_panel ~label:"Send to Editors" ~multi:true ~classes:[ "Editors" ]
    ("editors" ^ id)
    (List.rev_map fst (Uuid.Map.bindings model.editors))
    (fun editor_id -> [ Node.text (Uuid.Id.show editor_id) ])
    [
      Gui.button ctx "Send (ctrl-s)" tabindexes ~on_click:(fun () ->
          Gui.send model ctx.editor);
      Gui.none_button ctx "All" tabindexes ~on_click:(fun () ->
          Js.fill_selection ("editors" ^ id));
      Gui.none_button ctx "None" tabindexes ~on_click:(fun () ->
          Js.clear_selection ("editors" ^ id));
    ]

let multiparented_panel (ctx : Gui.context) (id : string) (mp : Tree.t list) :
    Node.t =
  (* TODO: fix buggy cursor inside Multiparented box *)
  Gui.select_panel ~label:"Multiparented" ~multi:false ("multiparent" ^ id) mp
    (fun tree ->
      let vertex =
        match tree with
        | Ref vertex -> (
            match Graph.vertex ctx.editor.graph vertex.id with
            | None -> failwith __LOC__
            | Some v -> v )
        | Con (vertex, _) -> vertex
      in
      view_tree ctx None tree ~at_top:true ~with_parens:false
      :: ( Graph.parent_vertexes ctx.editor.graph vertex
         |> Vertex.Set.elements
         |> List.map (fun vertex ->
                Tree.reachable
                  (Graph.live_edges ctx.editor.graph)
                  (Graph.multiparented ctx.editor.graph)
                  vertex
                |> fst
                |> view_tree ctx None ~at_top:true ~with_parens:false) ))
    []

let deleted_panel (ctx : Gui.context) (id : string)
    (tabindexes : int Uuid.Map.t) (deleted : Vertex.Set.t) (d : Tree.t list) :
    Node.t =
  Gui.select_panel ~label:"Deleted" ~multi:false ("deleted" ^ id) d
    (fun tree -> [ view_tree ctx None tree ~at_top:true ~with_parens:false ])
    [
      ( Js.set_input ("restore" ^ id) "";
        Gui.panel
          [
            Gui.button ctx "Restore" tabindexes ~on_click:(fun () ->
                Gui.restore ctx.editor deleted (Js.get_input ("restore" ^ id)));
            Gui.text_input ctx ("restore" ^ id) tabindexes
              ~on_change:(fun str -> Gui.restore ctx.editor deleted str);
          ] );
    ]

let simple_cycles_panel (ctx : Gui.context) (id : string) (sc : Tree.t list) :
    Node.t =
  Gui.select_panel ~label:"Simple Cycles" ~multi:false ("cycles" ^ id) sc
    (fun tree -> [ view_tree ctx None tree ~with_parens:false ])
    []

(* Views *)

let view_editor (model : Model.t) (ctx : Gui.context)
    (tabindexes : int Uuid.Map.t) : Node.t =
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
        (Key.dispatch model ctx.editor tabindexes ~inject:ctx.inject);
    ]
    [
      (* MAIN CODE VIEW *)
      view_tree ctx None r ~at_top:true ~with_parens:false;
      (* -------------- *)
      Gui.break;
      cursor_panel ctx;
      graph_panel ctx id tabindexes;
      Node.div
        [ Attr.class_ "selectors" ]
        [
          actions_panel ctx id;
          send_actions_panel model ctx id tabindexes;
          multiparented_panel ctx id mp;
          deleted_panel ctx id tabindexes tree_ctx.deleted d;
          simple_cycles_panel ctx id sc;
        ];
      Gui.panel ~label:"Patterns and Expressions"
        [
          Gui.action_button ctx "Pat (p)" Lang.Sort.Pat tabindexes
            (Some "pat_id") (fun str -> Edit (Create (Exp_var str)));
          Gui.action_button ctx "Var (v)" Lang.Sort.Exp tabindexes
            (Some "var_id") (fun str -> Edit (Create (Exp_var str)));
          Gui.action_button ctx "Num (n)" Lang.Sort.Exp tabindexes
            (Some "num_id") (fun str ->
              Edit (Create (Exp_num (int_of_string str))));
          Gui.action_button ctx "Lam (\\)" Lang.Sort.Exp tabindexes None
            (fun _ -> Edit (Create Exp_lam));
          Gui.action_button ctx "App (space)" Lang.Sort.Exp tabindexes None
            (fun _ -> Edit (Create Exp_app));
          Gui.action_button ctx "Plus (+)" Lang.Sort.Exp tabindexes None
            (fun _ -> Edit (Create Exp_plus));
          Gui.action_button ctx "Times (*)" Lang.Sort.Exp tabindexes None
            (fun _ -> Edit (Create Exp_times));
        ];
      Gui.panel ~label:"Types"
        [
          Gui.action_button ctx "Num (N)" Lang.Sort.Typ tabindexes None
            (fun _ -> Edit (Create Typ_num));
          Gui.action_button ctx "Arrow (>)" Lang.Sort.Typ tabindexes None
            (fun _ -> Edit (Create Typ_arrow));
        ];
      Gui.panel ~label:"Cursor"
        [
          Gui.button ctx "Delete (delete)" tabindexes ~on_click:(fun () ->
              Js.clear_selection ("deleted" ^ id);
              Some (Edit Destroy));
          Gui.some_button ctx "Up (↑)" tabindexes (Move Up);
          Gui.some_button ctx "Down (↓)" tabindexes (Move Down);
          Gui.some_button ctx "Left (←)" tabindexes (Move Left);
          Gui.some_button ctx "Right (→)" tabindexes (Move Right);
          Gui.button ctx "Teleport" tabindexes ~on_click:(Gui.teleport ctx id);
        ];
      Gui.panel ~label:"Environment"
        [
          Gui.some_button ctx "Record" tabindexes (Env Record)
            ~disabled:(Option.is_some model.actions);
          Gui.some_button ctx "Report" tabindexes (Env Report)
            ~disabled:(Option.is_none model.actions);
          Gui.some_button ctx "Stop" tabindexes (Env Stop)
            ~disabled:(Option.is_none model.actions);
          Gui.button ctx "Replay" tabindexes ~on_click:(fun () ->
              Some (Env (Replay (Js.prompt "Replay Recording"))));
          Gui.some_button ctx "Dump" tabindexes (Env Dump);
          Gui.button ctx "Load" tabindexes ~on_click:(fun () ->
              Some (Env (Load (Js.prompt "Load a Dump"))));
        ];
      Gui.panel ~label:"Editor"
        [
          Gui.some_button ctx "Clone" tabindexes (Env (Clone ctx.editor.id));
          Gui.some_button ctx "Drop" tabindexes (Env (Drop ctx.editor.id))
            ~disabled:(Uuid.Map.cardinal model.editors < 2);
        ];
    ]

let view ~(inject : Action.t -> Vdom.Event.t) (model : Model.t) : Node.t =
  let editors = Uuid.Map.bindings model.editors |> List.map snd in
  let editor_ids = List.map (fun (editor : Editor.t) -> editor.id) editors in
  let indexes = List.init (List.length editors) (fun i -> i + 1) in
  let tabindexes =
    List.combine editor_ids indexes |> List.to_seq |> Uuid.Map.of_seq
  in
  Node.div
    [ Attr.create "tabindex" "-1" ]
    (List.map
       (fun editor -> view_editor model { inject; editor } tabindexes)
       editors)
