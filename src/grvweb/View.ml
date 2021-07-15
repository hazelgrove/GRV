module Vdom = Virtual_dom.Vdom
module Node = Virtual_dom.Vdom.Node
module Attr = Virtual_dom.Vdom.Attr

let black (str : string) : Node.t =
  Node.span [ Attr.classes [ "string" ] ] [ Node.text str ]

let red (str : string) : Node.t =
  Node.span [ Attr.classes [ "error" ] ] [ Node.text str ]

let parens (node : Node.t) : Node.t =
  Node.span [] [ black "("; node; black ")" ]

module Context = struct
  type t = { tabindexes : int Id.Map.t; editor_id : Editor.id }
end

(* let cursor_node (node : Node.t) : Node.t =
  Node.span [ Attr.class_ "cursor" ] [ node ] *)

(* let maybe_cursor_node (editor : Editor.t) (parent : Cursor.t) (node : Node.t) :
    Node.t =
  if parent = editor.cursor then cursor_node node else node *)

(* let ref_node (ctx : Gui.context) (parent : Cursor.t) (id : Uuid.Id.t) : Node.t =
  Node.span
    [ Attr.class_ "vertex"; Gui.clicks_to ctx parent ]
    [ Node.text ("#" ^ Uuid.Id.to_string id) ] *)

(* let hole_node (ctx : Gui.context) (parent : Cursor.t) : Node.t =
   Node.span [ Attr.class_ "hole"; Gui.clicks_to ctx parent ] [ chars "□" ] *)

(* let rec tree_node ?(parent : Cursor.t = Cursor.root) ?(at_top : bool = true)
    (ctx : Gui.context) (tree : Tree.t) : Node.t =
  match tree with
  | Ref vertex -> ref_node ctx parent vertex.id
  | Vertex (vertex, children) -> vertex_node ctx parent vertex children at_top
  |> maybe_cursor_node ctx.editor parent *)

(* and vertex_node (ctx : Gui.context) (parent : Cursor.t) (vertex : Old_Vertex.t)
    (children : Tree.children Position_map.t) (at_top : bool) : Node.t =
  let node =
    Node.span
      [ Gui.clicks_to ctx parent ]
      (Lang.show chars chars
         (fun position ->
           match Position_map.get position children with
           | [] ->
               hole_node ctx { vertex; position }
               (* |> maybe_cursor_node ctx.editor { vertex; position } *)
           | [ { tree; _ } ] ->
               let at_top = vertex.value = Exp_lam in
               tree_node ctx tree ~at_top ~parent:{ vertex; position }
           | child_specs ->
               conflict_node ctx Cursor.{ vertex; position } child_specs)
         vertex.value)
  in
  let decorated_node =
    if ctx.editor.show_ids && not (vertex = Old_Vertex.root) then
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
    | _ -> decorated_node *)

(* and conflict_node (ctx : Gui.context) (parent : Cursor.t)
    (child_specs : Tree.children) : Node.t =
  let nodes =
    List.map
      (fun (child : Tree.child) -> tree_node ctx child.tree ~parent)
      child_specs
  in
  Node.span
    [ Attr.class_ "conflict"; Gui.clicks_to ctx parent ]
    ([ errs "{ " ] @ Util.List.intersperse (errs " | ") nodes @ [ errs " }" ])
|> maybe_cursor_node ctx.editor parent *)

(* Panels *)

(* let cursor_panel (ctx : Gui.context) : Node.t =
  Gui.panel ~label:"Cursor" [ chars (Cursor.to_string ctx.editor.cursor) ] *)

(* let graph_panel (ctx : Gui.context) (id : string) (tabindexes : int Uuid.Map.t)
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
    ] *)

(* let actions_panel (ctx : Gui.context) (id : string) : Node.t =
  Gui.select_panel ~label:"Actions" ~multi:true ("actions" ^ id)
    (Graph_action.Set.elements ctx.editor.actions)
    (fun graph_action -> [ chars (Graph_action.to_string graph_action) ])
    [] *)

(* let send_actions_panel (model : Model.t) (ctx : Gui.context) (id : string)
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
    ] *)

(* let multiparented_panel (ctx : Gui.context) (id : string) (mp : Tree.t list)
    (children : Old_Edge.Set.t Old_Vertex.Map.t) : Node.t =
  (* TODO: fix buggy cursor inside Multiparented box *)
  Gui.select_panel ~label:"Multiparented" ~multi:false ("multiparent" ^ id) mp
    (fun tree ->
      let vertex = match tree with Ref v | Vertex (v, _) -> v in
      tree_node ctx tree
      ::
      (Old_Graph.parent_vertexes ctx.editor.graph vertex
      |> Old_Vertex.Set.elements
      |> List.map (fun parent_vertex ->
             Old_Grove.traverse_vertex parent_vertex children
               ~seen:(Old_Vertex.Set.singleton vertex)
             |> (function tree, _, _ -> tree)
             |> tree_node ctx)))
    [] *)

(* let deleted_panel (ctx : Gui.context) (id : string)
    (tabindexes : int Uuid.Map.t) (deleted : Tree.t list) : Node.t =
  let d =
    List.map Tree.(function Vertex (v, _) | Ref v -> v) deleted
    |> Old_Vertex.Set.of_list
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
    ] *)

(* let unicycles_panel (ctx : Gui.context) (id : string) (sc : Tree.t list) :
    Node.t =
  Gui.select_panel ~label:"Unicycles" ~multi:false ("unicycles" ^ id) sc
    (fun tree -> [ tree_node ctx tree ])
    [] *)

(* Views *)

(* let view_editor (model : Model.t) (ctx : Gui.context)
    (tabindexes : int Uuid.Map.t) : Node.t =
  let id : string = Uuid.Id.to_string ctx.editor.id in
  let decomp, children = Old_Grove.decompose ctx.editor.graph in
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
    ] *)

let view_pat (_ctx : Context.t) (_pat : Pat.t) : Node.t = Node.text "PAT"

let view_typ (_ctx : Context.t) (_typ : Typ.t) : Node.t = Node.text "TYP"

let rec view_exp (ctx : Context.t) (exp : Exp.t) : Node.t =
  let view_exp_var x =
    Node.div [ Attr.classes [ "Exp"; "Var" ] ] [ Node.text x ]
  in

  let view_exp_lam pat typ exp' =
    let pat_node = view_pat ctx pat in
    let typ_node = view_typ ctx typ in
    let exp_node = view_exp ctx exp' in
    Node.div
      [ Attr.classes [ "Exp"; "Lam" ] ]
      [ black "λ"; pat_node; black ":"; typ_node; black "."; exp_node ]
  in

  let view_exp_app exp1 exp2 =
    let exp1_node = view_exp ctx exp1 in
    let exp2_node = view_exp ctx exp2 in
    Node.div
      [ Attr.classes [ "Exp"; "App" ] ]
      [ exp1_node; black " "; exp2_node ]
  in

  let view_exp_num n =
    let n_str = Int.to_string n in
    Node.div [ Attr.classes [ "Exp"; "Num" ] ] [ Node.text n_str ]
  in

  let view_exp_plus exp1 exp2 =
    let exp1_node = view_exp ctx exp1 in
    let exp2_node = view_exp ctx exp2 in
    Node.div
      [ Attr.classes [ "Exp"; "Plus" ] ]
      [ exp1_node; black "+"; exp2_node ]
  in

  let view_exp_times exp1 exp2 =
    let exp1_node = view_exp ctx exp1 in
    let exp2_node = view_exp ctx exp2 in
    Node.div
      [ Attr.classes [ "Exp"; "Times" ] ]
      [ exp1_node; black "×"; exp2_node ]
  in

  let view_exp_multiparent (edge : Edge.t) =
    let target_id = Id.to_string edge.target.id in
    Node.div
      [ Attr.classes [ "Exp"; "Multiparent" ] ]
      [ red "⩛"; black target_id ]
  in

  let view_exp_unicycle (edge : Edge.t) =
    let target_id = Id.to_string edge.target.id in
    Node.div
      [ Attr.classes [ "Exp"; "Unicycle" ] ]
      [ red "⟲"; black target_id ]
  in

  let view_exp_conflict conflict =
    let rec intersperse sep lst =
      match lst with
      | [] | [ _ ] -> lst
      | head :: tail -> head :: sep :: intersperse sep tail
    in
    let exp_nodes =
      conflict |> Exp.C.elements
      |> List.map (view_exp ctx)
      |> intersperse (red "|")
    in
    Node.div
      [ Attr.classes [ "Exp"; "Conflict" ] ]
      (red "{" :: exp_nodes @ [ red "}" ])
  in

  let view_exp_hole _vertex _position =
    Node.div [ Attr.classes [ "Exp"; "Hole" ] ] [ Node.text "□" ]
  in

  match exp with
  | Var (_, x) -> view_exp_var x
  | Lam (_, pat, typ, exp') -> view_exp_lam pat typ exp'
  | App (_, exp1, exp2) -> view_exp_app exp1 exp2
  | Num (_, n) -> view_exp_num n
  | Plus (_, exp1, exp2) -> view_exp_plus exp1 exp2
  | Times (_, exp1, exp2) -> view_exp_times exp1 exp2
  | Multiparent edge -> view_exp_multiparent edge
  | Unicycle edge -> view_exp_unicycle edge
  | Conflict conflict -> view_exp_conflict conflict
  | Hole (vertex, position) -> view_exp_hole vertex position

let view_term (_ctx : Context.t) (_term : Term.t) : Node.t = Node.text "TERM"

let view_term_set (_ctx : Context.t) (_terms : Term.Set.t) : Node.t =
  Node.text "TERM_SET"

let view_zpat (_ctx : Context.t) (_zpat : ZPat.t) : Node.t = Node.text "ZPAT"

let view_ztyp (_ctx : Context.t) (_ztyp : ZTyp.t) : Node.t = Node.text "ZTYP"

let rec view_zexp (ctx : Context.t) (zexp : ZExp.t) : Node.t =
  match zexp with
  | Cursor exp ->
      let attr = Attr.classes [ "Cursor" ] in
      let exp_node = view_exp ctx exp in
      Node.div [ attr ] [ exp_node ]
  | ZLamP (_, zpat, typ, exp) ->
      let zpat_node = view_zpat ctx zpat in
      let typ_node = view_typ ctx typ in
      let exp_node = view_exp ctx exp in
      let attr = Attr.classes [ "ZLamP" ] in
      Node.div [ attr ] [ zpat_node; typ_node; exp_node ]
  | ZLamT (_, pat, ztyp, exp) ->
      let pat_node = view_pat ctx pat in
      let ztyp_node = view_ztyp ctx ztyp in
      let exp_node = view_exp ctx exp in
      let attr = Attr.classes [ "ZLamT" ] in
      Node.div [ attr ] [ pat_node; ztyp_node; exp_node ]
  | ZLamE (_, pat, typ, zexp) ->
      let pat_node = view_pat ctx pat in
      let typ_node = view_typ ctx typ in
      let zexp_node = view_zexp ctx zexp in
      let attr = Attr.classes [ "ZLamE" ] in
      Node.div [ attr ] [ pat_node; typ_node; zexp_node ]
  | ZAppE1 (_, zexp1, exp2) ->
      let zexp1_node = view_zexp ctx zexp1 in
      let exp2_node = view_exp ctx exp2 in
      let attr = Attr.classes [ "ZAppE1" ] in
      Node.div [ attr ] [ zexp1_node; exp2_node ]
  | ZAppE2 (_, exp1, zexp2) ->
      let exp1_node = view_exp ctx exp1 in
      let zexp2_node = view_zexp ctx zexp2 in
      let attr = Attr.classes [ "ZAppE2" ] in
      Node.div [ attr ] [ exp1_node; zexp2_node ]
  | ZPlusE1 (_, zexp1, exp2) ->
      let zexp1_node = view_zexp ctx zexp1 in
      let exp2_node = view_exp ctx exp2 in
      let attr = Attr.classes [ "ZPlusE1" ] in
      Node.div [ attr ] [ zexp1_node; exp2_node ]
  | ZPlusE2 (_, exp1, zexp2) ->
      let exp1_node = view_exp ctx exp1 in
      let zexp2_node = view_zexp ctx zexp2 in
      let attr = Attr.classes [ "ZPlusE2" ] in
      Node.div [ attr ] [ exp1_node; zexp2_node ]
  | ZTimesE1 (_, zexp1, exp2) ->
      let zexp1_node = view_zexp ctx zexp1 in
      let exp2_node = view_exp ctx exp2 in
      let attr = Attr.classes [ "ZTimesE1" ] in
      Node.div [ attr ] [ zexp1_node; exp2_node ]
  | ZTimesE2 (_, exp1, zexp2) ->
      let exp1_node = view_exp ctx exp1 in
      let zexp2_node = view_zexp ctx zexp2 in
      let attr = Attr.classes [ "ZTimesE2" ] in
      Node.div [ attr ] [ exp1_node; zexp2_node ]
  | ZConflict (zexp, conflict) ->
      let zexp_node = view_zexp ctx zexp in
      let exp0 = ZExp.erase_cursor zexp in
      let nodes =
        let view' exp =
          if Exp.equal exp exp0 then zexp_node else view_exp ctx exp
        in
        conflict |> Exp.C.elements |> List.map view'
      in
      let attr = Attr.classes [ "ZConflict" ] in
      Node.div [ attr ] nodes

let view_zterm (ctx : Context.t) (zterm : ZTerm.t) : Node.t =
  match zterm with
  | ZExp zexp -> view_zexp ctx zexp
  | ZPat zpat -> view_zpat ctx zpat
  | ZTyp ztyp -> view_ztyp ctx ztyp

let view_zterm_set (_ctx : Context.t) (_zterms : ZTerm.Set.t) : Node.t =
  Node.text "ZTERM_SET"

let view_zgrove (ctx : Context.t) (zgrove : ZGrove.t) : Node.t =
  let root_opt = ZGrove.find_root_opt zgrove in
  let root_node =
    let classes, nodes =
      match root_opt with
      | None -> ([ "Error" ], [ Node.text "NO_ROOT" ])
      | Some (Root term) -> ([ "Inactive" ], [ view_term ctx term ])
      | Some (ZRoot zterm) -> ([ "Active" ], [ view_zterm ctx zterm ])
    in
    Node.div [ Attr.classes ("Root" :: classes) ] nodes
  in
  let noparents_label = Node.h3 [] [ Node.text "Deleted" ] in
  let multiparents_label = Node.h3 [] [ Node.text "Multiparented" ] in
  let unicycles_label = Node.h3 [] [ Node.text "Unicycles" ] in
  let classes, noparents_node, multiparents_node, unicycles_node =
    let noparents_node =
      match zgrove with
      | ZGroveTS1 { znoparents; _ } ->
          let classes =
            match root_opt with
            | None -> [ "Noparents"; "Active" ]
            | Some _ -> [ "Noparents"; "Inactive" ]
          in
          Node.div [ Attr.classes classes ]
            [ noparents_label; view_zterm_set ctx znoparents ]
      | ZGroveTS2 { noparents; _ } | ZGroveTS3 { noparents; _ } ->
          Node.div
            [ Attr.classes [ "Noparents"; "Inactive" ] ]
            [ noparents_label; view_term_set ctx noparents ]
    in
    let multiparents_node =
      match zgrove with
      | ZGroveTS2 { zmultiparents; _ } ->
          Node.div
            [ Attr.classes [ "Multiparents"; "Active" ] ]
            [ multiparents_label; view_zterm_set ctx zmultiparents ]
      | ZGroveTS1 { multiparents; _ } | ZGroveTS3 { multiparents; _ } ->
          Node.div
            [ Attr.classes [ "Multiparents"; "Inactive" ] ]
            [ multiparents_label; view_term_set ctx multiparents ]
    in
    let unicycles_node =
      match zgrove with
      | ZGroveTS3 { zunicycles; _ } ->
          Node.div
            [ Attr.classes [ "Unicycles"; "Active" ] ]
            [ unicycles_label; view_zterm_set ctx zunicycles ]
      | ZGroveTS1 { unicycles; _ } | ZGroveTS2 { unicycles; _ } ->
          Node.div
            [ Attr.classes [ "Unicycles"; "Inactive" ] ]
            [ unicycles_label; view_term_set ctx unicycles ]
    in
    let classes =
      match zgrove with
      | ZGroveTS1 _ -> [ "ZGroveTS1" ]
      | ZGroveTS2 _ -> [ "ZGroveTS2" ]
      | ZGroveTS3 _ -> [ "ZGroveTS3" ]
    in
    (classes, noparents_node, multiparents_node, unicycles_node)
  in
  Node.div
    [ Attr.classes ("ZGrove" :: classes) ]
    [ root_node; noparents_node; multiparents_node; unicycles_node ]

let view_editor (ctx : Context.t) (editor : Editor.t) : Node.t =
  let editor_cssid = "Editor" ^ Id.to_string editor.id in
  let label_node = Node.h2 [] [ Node.text editor_cssid ] in
  let code_node = view_zgrove ctx editor.zgrove in
  let classes = Attr.classes [ "Editor" ] in
  let cssid = Attr.id editor_cssid in
  let tabindex =
    match Id.Map.find_opt editor.id ctx.tabindexes with
    | Some tabindex -> [ Attr.create "tabindex" (Id.to_string tabindex) ]
    | None -> []
  in
  let attrs = classes :: cssid :: tabindex in
  Node.div attrs [ label_node; code_node ]

let view ~inject:(_ : Action.t -> Vdom.Event.t) (model : Model.t) : Node.t =
  let editor_ids, editors = model.editors |> Id.Map.bindings |> List.split in
  let tabindexes =
    List.init (List.length editors) (fun i -> i + 1)
    |> List.combine editor_ids |> List.to_seq |> Id.Map.of_seq
  in
  let header_node = Node.h1 [] [ Node.text "Grove" ] in
  let editor_nodes =
    let view' editor_id editor =
      let ctx = Context.{ tabindexes; editor_id } in
      view_editor ctx editor
    in
    List.map2 view' editor_ids editors
  in
  let editors_node = Node.div [ Attr.classes [ "Editors" ] ] editor_nodes in
  let attr = Attr.create "tabindex" "-1" in
  Node.div [ attr ] [ header_node; editors_node ]
