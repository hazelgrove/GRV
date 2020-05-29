module Dom = Js_of_ocaml.Dom
module Dom_html = Js_of_ocaml.Dom_html
module Vdom = Virtual_dom.Vdom
module W = Widget

let rec intersperse (delim : 'a) (xs : 'a list) : 'a list =
  match xs with
  | [] | [ _ ] -> xs
  | x :: xs' -> x :: delim :: intersperse delim xs'

let clickable ~inject (model : Model.Instance.t) (cursor : Cursor.t) :
    Vdom.Attr.t =
  Vdom.Attr.on_click (fun event ->
      Dom.preventDefault event;
      Dom_html.stopPropagation event;
      inject { Action.instance_id = model.id; action = Select cursor })

let rec of_index ~inject (model : Model.Instance.t) (child : Cursor.t) :
    Vdom.Node.t =
  let open Vdom.Node in
  let open Vdom.Attr in
  let node =
    let recur : Vertex.t -> Cursor.t -> Vdom.Node.t = of_vertex ~inject model in
    match Edge.Set.elements (Cache.children child model.graph.cache) with
    | [] ->
        span [ class_ "hole"; clickable ~inject model child ] [ W.chars "_" ]
    | [ edge ] -> recur edge.target child
    | edges ->
        let nodes =
          List.map (fun (edge : Edge.t) -> recur edge.target child) edges
        in
        span
          [ class_ "conflict"; clickable ~inject model child ]
          ([ W.errs "{" ] @ intersperse (W.errs "|") nodes @ [ W.errs "}" ])
  in
  if model.cursor = child then span [ class_ "cursor" ] [ node ] else node

and of_vertex ~inject (model : Model.Instance.t) (vertex : Vertex.t)
    (parent : Cursor.t) : Vdom.Node.t =
  let open Vdom.Node in
  let open Vdom.Attr in
  let node =
    let recur (index : Lang.Index.t) : Vdom.Node.t =
      of_index ~inject model { vertex; index }
    in
    span
      [ clickable ~inject model parent ]
      (Lang.show W.chars W.chars recur vertex.value)
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
      id @@ "instance" ^ Int.to_string this_model.id;
      class_ "instance";
      tabindex this_model.id;
      on_keydown @@ Key.dispatch ~inject model this_model;
    ]
    [
      of_index ~inject this_model Cursor.root;
      br [];
      br [];
      div []
        [
          mk
          @@ W.input_button "Pat (p)" "pat_id" Lang.Sort.Pat (fun str ->
                 Pat_var str);
          mk
          @@ W.input_button "Var (v)" "var_id" Lang.Sort.Exp (fun str ->
                 Exp_var str);
          mk
          @@ W.input_button "Num (n)" "num_id" Lang.Sort.Exp (fun str ->
                 Exp_num (int_of_string str));
          mk @@ W.create_button "Lam (\\)" Exp_lam Lang.Sort.Exp;
          mk @@ W.create_button "App (space)" Exp_app Lang.Sort.Exp;
          mk @@ W.create_button "Plus (+)" Exp_plus Lang.Sort.Exp;
          mk @@ W.create_button "Num (N)" Typ_num Lang.Sort.Typ;
          mk @@ W.create_button "Arrow (>)" Typ_arrow Lang.Sort.Typ;
        ];
      div []
        [
          mk @@ W.app_button "Delete (delete)" (Enqueue (Edit Destroy));
          mk @@ W.button "Send (ctrl-s)" (fun () -> Key.send this_model);
          mk @@ W.move_button "In (↓)" In;
          mk @@ W.move_button "Out (↑)" Out;
          mk @@ W.move_button "Left (←)" Left;
          mk @@ W.move_button "Right (→)" Right;
        ];
      mk
      @@ W.select
           ("actions" ^ Int.to_string this_model.id)
           "Actions" this_model.actions
           (fun action ->
             W.chars @@ Format.asprintf "%a" Graph_action.pp action);
      div [ class_ "select" ]
        ( [ h2 [] [ text "Deleted" ] ]
        @
        match this_model.graph.cache.deleted with
        | None -> []
        | Some edge ->
            [
              W.chars "[deleted ";
              of_vertex ~inject this_model edge.target edge.source;
              W.chars "]";
            ] );
      h2 [] [ text "Cursor" ];
      W.chars @@ Format.asprintf "%a@." Cursor.pp this_model.cursor;
      h2 [] [ text "Graph" ];
      div [ id @@ Printf.sprintf "graph%d" this_model.id ] [ span [] [] ];
    ]

let view ~(inject : Action.t -> Vdom.Event.t) (model : Model.t) : Vdom.Node.t =
  Vdom.Node.div []
    (List.map
       (fun (_, this_model) -> view_instance ~inject model this_model)
       (Model.MapInt.bindings model))
