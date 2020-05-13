module Dom_html = Js_of_ocaml.Dom_html
module Js = Js_of_ocaml.Js
module Vdom = Virtual_dom.Vdom

(* TODO: use a fmt for efficiency? *)
let rec of_index (graph : Graph.t) (cursor : Graph.Child.t)
    (child : Graph.Child.t) : string =
  let result =
    match Edge.Set.elements (Graph.find_children child graph) with
    | [] -> "__"
    | [ edge ] -> of_vertex graph cursor (Edge.target edge)
    | edges ->
        Printf.sprintf "{ %s }"
          (String.concat " | "
             (List.map
                (fun edge -> of_vertex graph cursor (Edge.target edge))
                edges))
  in
  if cursor = child then Printf.sprintf "<%s>" result else result

and of_vertex (graph : Graph.t) (cursor : Graph.Child.t) (vertex : Vertex.t) :
    string =
  let result =
    match vertex.value with
    | Exp_app ->
        Printf.sprintf "(%s %s)"
          (of_index graph cursor { parent = vertex; index = Exp_app_fun })
          (of_index graph cursor { parent = vertex; index = Exp_app_arg })
    | _ -> "TODO"
  in
  Printf.sprintf "%s:%s" (Uuid.Id.show vertex.id) result

let view_instance (instance : int) ~(inject : Action.t -> Vdom.Event.t)
    (model : Model.Instance.t) : Vdom.Node.t =
  let open Action in
  let open Vdom.Node in
  let open Vdom.Attr in
  let key_action event : Action.inst =
    if Js.to_bool event##.ctrlKey then NoOp
    else
      match Dom_html.Keyboard_code.of_event event with
      | Space -> Create
      | ArrowUp -> Move Out
      | ArrowDown -> Move In
      | ArrowLeft -> Move Left
      | ArrowRight -> Move Right
      | _ -> NoOp
  in
  let action_button (label : string) (action : Action.app) : Vdom.Node.t =
    button [ on_click (fun _ -> inject { instance; action }) ] [ text label ]
  in
  div
    [
      tabindex instance;
      on_keydown (fun event ->
          Js_of_ocaml.Dom.preventDefault event;
          inject { instance; action = Enqueue (key_action event) });
    ]
    [
      text (of_index model.graph model.cursor Graph.Child.root);
      br [];
      br [];
      action_button "App" (Enqueue Create);
      action_button "Send" Send;
      action_button "In" (Enqueue (Move In));
      action_button "Out" (Enqueue (Move Out));
      action_button "Left" (Enqueue (Move Left));
      action_button "Right" (Enqueue (Move Right));
      br [];
      br [];
      select
        [ create "size" "10"; bool_property "multiple" true; disabled ]
        (List.rev_map
           (fun action ->
             option [] [ text @@ Format.asprintf "%a" Graph_action.pp action ])
           model.actions);
      br [];
      br [];
      text "Cursor";
      br [];
      pre [] [ text @@ Format.asprintf "%a@." Graph.Child.pp model.cursor ];
      br [];
      text "Graph";
      br [];
      pre [] [ text @@ Format.asprintf "%a@." Graph.pp_graph model.graph ];
    ]

let view ~(inject : Action.t -> Vdom.Event.t) (model : Model.t) : Vdom.Node.t =
  Vdom.Node.div []
    (List.map
       (fun (i, m) -> view_instance i m ~inject)
       (Model.MapInt.bindings model))
