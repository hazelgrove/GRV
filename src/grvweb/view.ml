let rec of_hexp (hexp : Ast.HExp.t) (cursor_opt : Cursor.t option) : string =
  let open Ast.HExp in
  let open Cursor in
  let open Printf in
  match cursor_opt with
  | None -> (
      match hexp.value with
      | EmptyHole -> "__"
      | App (e1, e2) -> sprintf "(%s %s)" (of_hexp e1 None) (of_hexp e2 None) )
  | Some Here -> sprintf "<%s>" (of_hexp hexp None)
  | Some (To (k, cursor)) -> (
      match hexp.value with
      | EmptyHole -> "__"
      | App (e1, e2) ->
          let e1' = of_hexp e1 (if k == Left then Some cursor else None)
          and e2' = of_hexp e2 (if k == Right then Some cursor else None) in
          sprintf "(%s %s)" e1' e2' )

let view ~(inject : Action.t -> Virtual_dom.Vdom.Event.t) (model : Model.t) =
  let open Action in
  let open Virtual_dom.Vdom.Node in
  let open Virtual_dom.Vdom.Attr in
  div []
    [
      text (of_hexp model.ast (Some model.cursor));
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
      pre []
        [ text @@ Format.asprintf "%a@." Graph.VertexIndex.pp model.cursor_ref ];
      br [];
      text "Graph";
      br [];
      pre [] [ text @@ Format.asprintf "%a@." Graph.pp_graph model.graph ];
    ]
