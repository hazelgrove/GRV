module Dom = Js_of_ocaml.Dom
module Dom_html = Js_of_ocaml.Dom_html
module Vdom = Virtual_dom.Vdom
module W = Widget

let clickable ~inject (model : Model.Instance.t) (cursor : Cursor.t) :
    Vdom.Attr.t =
  Vdom.Attr.on_click (fun event ->
      Dom.preventDefault event;
      Dom_html.stopPropagation event;
      inject { Action.instance_id = model.id; action = Select cursor })

let rec view_cursor ~inject (model : Model.Instance.t) (cursor : Cursor.t) :
    Vdom.Node.t =
  let open Vdom.Node in
  let open Vdom.Attr in
  let node =
    let view_vertex' : Vertex.t -> Cursor.t option -> Vdom.Node.t =
      view_vertex ~inject model
    in
    match Edge.Set.elements (Cache.children cursor model.value.graph.cache) with
    | [] ->
        span [ class_ "hole"; clickable ~inject model cursor ] [ W.chars "_" ]
    | [ edge ] -> view_vertex' (Edge.target edge) (Some cursor)
    | edges ->
        let nodes =
          List.map
            (fun (edge : Edge.t) ->
              view_vertex' (Edge.target edge) (Some cursor))
            edges
        in
        span
          [ class_ "conflict"; clickable ~inject model cursor ]
          ( [ W.errs "{" ]
          @ Util.List.intersperse (W.errs "|") nodes
          @ [ W.errs "}" ] )
  in
  if model.value.cursor = cursor then span [ class_ "cursor" ] [ node ]
  else node

and view_vertex ~inject (model : Model.Instance.t) (vertex : Vertex.t)
    (parent : Cursor.t option) : Vdom.Node.t =
  let open Vdom.Node in
  let open Vdom.Attr in
  let node =
    let view_cursor' (index : Lang.Index.t) : Vdom.Node.t =
      view_cursor ~inject model { vertex; index }
    in
    let attr =
      match parent with None -> [] | Some p -> [ clickable ~inject model p ]
    in
    span attr (Lang.show W.chars W.chars view_cursor' vertex.value)
  in
  span [ class_ "vertex" ]
    [ Vdom.Node.create "sub" [] [ text @@ Uuid.Id.show vertex.id ]; node ]

let view_instance ~(inject : Action.t -> Vdom.Event.t) (model : Model.t)
    (this_model : Model.Instance.t) : Vdom.Node.t =
  let open Action in
  let open Vdom.Node in
  let open Vdom.Attr in
  let mk (w : Vdom.Node.t W.t) : Vdom.Node.t = W.mk w ~inject this_model in
  Graphviz.draw this_model;
  div
    [
      id @@ "instance" ^ Uuid.Id.show this_model.id;
      class_ "instance";
      create "tabindex" (Uuid.Id.show this_model.id);
      on_keydown @@ Key.dispatch ~inject model this_model;
    ]
    [
      view_cursor ~inject this_model Cursor.root;
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
                 Js.clear_selection ("deleted" ^ Uuid.Id.show this_model.id);
                 Some (Enqueue (Edit Destroy)));
        ];
      div []
        [
          mk @@ W.move_button "In (↓)" In;
          mk @@ W.move_button "Out (↑)" Out;
          mk @@ W.move_button "Left (←)" Left;
          mk @@ W.move_button "Right (→)" Right;
        ];
      div [ class_ "selector" ]
        [
          mk
          @@ W.select
               ("actions" ^ Uuid.Id.show this_model.id)
               "Actions" this_model.value.actions
               (fun (item : Graph_action.t) ->
                 W.chars @@ Format.asprintf "%a" Graph_action.pp item);
          mk @@ W.button "Send (ctrl-s)" (fun () -> Key.send this_model);
        ];
      div [ class_ "selector" ]
        [
          mk
          @@ W.select ~multi:false ~default:false
               ("deleted" ^ Uuid.Id.show this_model.id)
               "Deleted"
               (Vertex.Set.elements
                  (Vertex.Set.remove Vertex.root
                     (Roots.roots this_model.value.graph)))
               (fun (vertex : Vertex.t) ->
                 view_vertex ~inject this_model vertex None);
          mk @@ W.button "Restore (ctrl-r)" (fun () -> Key.restore this_model);
        ];
      h2 [] [ text "Cursor" ];
      W.chars @@ Format.asprintf "%a@." Cursor.pp this_model.value.cursor;
      h2 [] [ text "Graph" ];
      div [ id ("graph" ^ Uuid.Id.show this_model.id) ] [ span [] [] ];
    ]

let view ~(inject : Action.t -> Vdom.Event.t) (model : Model.t) : Vdom.Node.t =
  Vdom.Node.div []
    (List.map
       (fun (_, this_model) -> view_instance ~inject model this_model)
       (Uuid.Map.bindings model))
