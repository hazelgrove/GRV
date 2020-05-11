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

let view_instance (instance : int)
    ~(inject : Action.t -> Virtual_dom.Vdom.Event.t) (model : Model.Instance.t)
    : Virtual_dom.Vdom.Node.t =
  let open Action in
  let open Virtual_dom.Vdom.Node in
  let open Virtual_dom.Vdom.Attr in
  let _make_key_action event =
    if Js_of_ocaml.Js.to_bool event##.ctrlKey then NoOp
    else
      match Js_of_ocaml.Dom_html.Keyboard_code.of_event event with
      | Space -> Create
      | ArrowUp -> Move Out
      | ArrowDown -> Move In
      | ArrowLeft -> Move Left
      | ArrowRight -> Move Right
      | _ -> NoOp
  in
  div [ (*on_keydown (fun event -> inject @@ make_key_action event)*) ]
    [
      text (of_index model.graph model.cursor Graph.Child.root);
      br [];
      br [];
      button
        [ on_click (fun _ -> inject { instance; action = Create }) ]
        [ text "App" ];
      button
        [ on_click (fun _ -> inject { instance; action = Send }) ]
        [ text "Send" ];
      button
        [ on_click (fun _ -> inject @@ { instance; action = Move In }) ]
        [ text "In" ];
      button
        [ on_click (fun _ -> inject @@ { instance; action = Move Out }) ]
        [ text "Out" ];
      button
        [ on_click (fun _ -> inject @@ { instance; action = Move Left }) ]
        [ text "Left" ];
      button
        [ on_click (fun _ -> inject @@ { instance; action = Move Right }) ]
        [ text "Right" ];
      br [];
      br [];
      pre []
        [
          text
          @@ Format.asprintf "%a@."
               (fun fmt ->
                 List.iter (fun action ->
                     Format.fprintf fmt "%a\n" Graph_action.pp action))
               model.actions;
        ];
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

let view ~(inject : Action.t -> Virtual_dom.Vdom.Event.t) (model : Model.t) :
    Virtual_dom.Vdom.Node.t =
  let open Virtual_dom.Vdom.Node in
  div []
    (List.map
       (fun (i, m) -> view_instance i m ~inject)
       (Model.MapInt.bindings model))
