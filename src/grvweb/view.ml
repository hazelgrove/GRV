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

let view ~(inject : Action.t -> Virtual_dom.Vdom.Event.t) (model : Model.t) =
  let open Action in
  let open Virtual_dom.Vdom.Node in
  let open Virtual_dom.Vdom.Attr in
  div []
    [
      text (of_index model.graph model.cursor Graph.Child.root);
      br [];
      br [];
      button [ on_click (fun _ -> inject Create) ] [ text "App" ];
      button [ on_click (fun _ -> inject @@ Move In) ] [ text "In" ];
      button [ on_click (fun _ -> inject @@ Move Out) ] [ text "Out" ];
      button [ on_click (fun _ -> inject @@ Move Left) ] [ text "Left" ];
      button [ on_click (fun _ -> inject @@ Move Right) ] [ text "Right" ];
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
