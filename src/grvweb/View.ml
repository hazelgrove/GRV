module Vdom = Virtual_dom.Vdom
module Node = Virtual_dom.Vdom.Node
module Attr = Virtual_dom.Vdom.Attr

(* Vdom Node Constructors *)

let chars (str : string) : Node.t =
  Node.span [ Attr.class_ "chars" ] [ Node.text str ]

let errs (str : string) : Node.t =
  Node.span [ Attr.class_ "errs" ] [ Node.text str ]

let parenthesized (node : Node.t) : Node.t =
  Node.span [] [ chars "("; node; chars ")" ]

let cursor_node (node : Node.t) : Node.t =
  Node.span [ Attr.class_ "cursor" ] [ node ]

let maybe_cursor_node (editor : Editor.t) (parent : Cursor.t) (node : Node.t) :
    Node.t =
  if parent = editor.cursor then cursor_node node else node

let ref_node (ctx : Gui.context) (parent : Cursor.t) (id : Uuid.Id.t) : Node.t =
  Node.span
    [ Attr.class_ "vertex"; Gui.clicks_to ctx parent ]
    [ Node.text ("#" ^ Uuid.Id.to_string id) ]

let hole_node (ctx : Gui.context) (parent : Cursor.t) : Node.t =
  Node.span [ Attr.class_ "hole"; Gui.clicks_to ctx parent ] [ chars "□" ]

let rec tree_node ?(parent : Cursor.t = Cursor.root) ?(at_top : bool = true)
    (ctx : Gui.context) (tree : Tree.t) : Node.t =
  (match tree with
  | Ref vertex -> ref_node ctx parent vertex.id
  | Vertex (vertex, children) -> vertex_node ctx parent vertex children at_top)
  |> maybe_cursor_node ctx.editor parent

and vertex_node (ctx : Gui.context) (parent : Cursor.t) (vertex : Vertex.t)
    (children : Tree.children Position_map.t) (at_top : bool) : Node.t =
  let node =
    Node.span
      [ Gui.clicks_to ctx parent ]
      (Lang.show chars chars
         (fun position ->
           match Position_map.get position children with
           | [] ->
               hole_node ctx { vertex; position }
               |> maybe_cursor_node ctx.editor { vertex; position }
           | [ { tree; _ } ] ->
               let at_top = vertex.value = Exp_lam in
               tree_node ctx tree ~at_top ~parent:{ vertex; position }
           | child_specs ->
               conflict_node ctx Cursor.{ vertex; position } child_specs)
         vertex.value)
  in
  let decorated_node =
    if ctx.editor.show_ids && not (vertex = Vertex.root) then
      Node.span [ Attr.class_ "vertex" ]
        [
          Node.create "sub" [] [ Node.text (Uuid.Id.to_string vertex.id ^ "(") ];
          node;
          Node.create "sub" [] [ Node.text ")" ];
        ]
    else Node.span [ Attr.class_ "vertex" ] [ node ]
  in
  if at_top || parent.position = Root_root_root then decorated_node
  else
    match vertex.value with
    | Exp_lam | Exp_app | Exp_plus | Exp_times | Typ_arrow ->
        parenthesized decorated_node
    | _ -> decorated_node

and conflict_node (ctx : Gui.context) (parent : Cursor.t)
    (child_specs : Tree.children) : Node.t =
  let nodes =
    List.map
      (fun (child : Tree.child) -> tree_node ctx child.tree ~parent)
      child_specs
  in
  Node.span
    [ Attr.class_ "conflict"; Gui.clicks_to ctx parent ]
    ([ errs "{ " ] @ Util.List.intersperse (errs " | ") nodes @ [ errs " }" ])
  |> maybe_cursor_node ctx.editor parent

(* Panels *)

let cursor_panel (ctx : Gui.context) : Node.t =
  Gui.panel ~label:"Cursor" [ chars (Cursor.to_string ctx.editor.cursor) ]

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
              | str -> Some (Edit (DropEdge (Uuid.Id.of_string str))));
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
    (fun graph_action -> [ chars (Graph_action.to_string graph_action) ])
    []

let send_actions_panel (model : Model.t) (ctx : Gui.context) (id : string)
    (tabindexes : int Uuid.Map.t) : Node.t =
  Gui.select_panel ~label:"Send to Editors" ~multi:true ~classes:[ "Editors" ]
    ("editors" ^ id)
    (List.rev_map fst (Uuid.Map.bindings model.editors))
    (fun editor_id -> [ Node.text (Uuid.Id.to_string editor_id) ])
    [
      Gui.button ctx "Send (ctrl-s)" tabindexes ~on_click:(fun () ->
          Gui.send model ctx.editor);
      Gui.none_button ctx "All" tabindexes ~on_click:(fun () ->
          Js.fill_selection ("editors" ^ id));
      Gui.none_button ctx "None" tabindexes ~on_click:(fun () ->
          Js.clear_selection ("editors" ^ id));
    ]

let multiparented_panel (ctx : Gui.context) (id : string) (mp : Tree.t list)
    (children : Edge.Set.t Vertex.Map.t) : Node.t =
  (* TODO: fix buggy cursor inside Multiparented box *)
  Gui.select_panel ~label:"Multiparented" ~multi:false ("multiparent" ^ id) mp
    (fun tree ->
      let vertex = match tree with Ref v | Vertex (v, _) -> v in
      tree_node ctx tree
      ::
      (Graph.parent_vertexes ctx.editor.graph vertex
      |> Vertex.Set.elements
      |> List.map (fun parent_vertex ->
             Grove.traverse_vertex parent_vertex children
               ~seen:(Vertex.Set.singleton vertex)
             |> (function tree, _, _ -> tree)
             |> tree_node ctx)))
    []

let deleted_panel (ctx : Gui.context) (id : string)
    (tabindexes : int Uuid.Map.t) (deleted : Tree.t list) : Node.t =
  let d =
    List.map Tree.(function Vertex (v, _) | Ref v -> v) deleted
    |> Vertex.Set.of_list
  in
  Gui.select_panel ~label:"Deleted" ~multi:false ("deleted" ^ id) deleted
    (function
      | (Vertex (vertex, _) | Ref vertex) as tree ->
          [
            tree_node ctx tree
              ~parent:
                {
                  vertex;
                  position =
                    Option.value
                      (Lang.Position.default_position vertex.value)
                      ~default:Lang.Position.Root_root_root;
                };
          ])
    [
      (Js.set_input ("restore" ^ id) "";
       Gui.panel
         [
           Gui.button ctx "Restore" tabindexes ~on_click:(fun () ->
               Gui.restore ctx.editor d (Js.get_input ("restore" ^ id)));
           Gui.text_input ctx ("restore" ^ id) tabindexes ~on_change:(fun str ->
               Gui.restore ctx.editor d str);
         ]);
    ]

let unicycles_panel (ctx : Gui.context) (id : string) (sc : Tree.t list) :
    Node.t =
  Gui.select_panel ~label:"Unicycles" ~multi:false ("unicycles" ^ id) sc
    (fun tree -> [ tree_node ctx tree ])
    []

(* Views *)

let view_editor (model : Model.t) (ctx : Gui.context)
    (tabindexes : int Uuid.Map.t) : Node.t =
  let id : string = Uuid.Id.to_string ctx.editor.id in
  let decomp, children = Grove.decompose ctx.editor.graph in
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
      tree_node ctx decomp.reachable;
      (* -------------- *)
      Gui.break;
      cursor_panel ctx;
      graph_panel ctx id tabindexes;
      Node.div
        [ Attr.class_ "selectors" ]
        [
          actions_panel ctx id;
          send_actions_panel model ctx id tabindexes;
          multiparented_panel ctx id decomp.multiparented children;
          deleted_panel ctx id tabindexes decomp.deleted;
          unicycles_panel ctx id decomp.wreaths;
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
  let positions = List.init (List.length editors) (fun i -> i + 1) in
  let tabindexes =
    List.combine editor_ids positions |> List.to_seq |> Uuid.Map.of_seq
  in
  Node.div
    [ Attr.create "tabindex" "-1" ]
    (List.map
       (fun editor -> view_editor model { inject; editor } tabindexes)
       editors)
