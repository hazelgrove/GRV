module Vdom = Virtual_dom.Vdom
open Virtual_dom.Vdom

let rec of_hexp (hexp : Ast.HExp.t) (cursor : Cursor.t option) : string =
  let open Ast.HExp in
  match cursor with
  | None -> (
      match hexp.value with
      | EmptyHole -> "[]"
      | App (e1, e2) ->
          Printf.sprintf "(%s %s)" (of_hexp e1 None) (of_hexp e2 None) )
  | Some [] -> Printf.sprintf "<%s>" (of_hexp hexp None)
  | Some (c :: cs) -> (
      match hexp.value with
      | EmptyHole -> "[]"
      | App (e1, e2) ->
          let e1' = of_hexp e1 (if c = 0 then Some cs else None)
          and e2' = of_hexp e2 (if c = 1 then Some cs else None) in
          Printf.sprintf "(%s %s)" e1' e2' )

let view ~(inject : Action.t -> Vdom.Event.t) (model : Model.t) =
  Node.div []
    [
      Node.text (of_hexp model.ast (Some model.cursor));
      Node.br [];
      Node.button
        [
          Attr.on_click
            (fun (_ : Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t) ->
              inject Action.App);
        ]
        [ Node.text "App" ];
      Node.button
        [
          Attr.on_click
            (fun (_ : Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t) ->
              inject Action.MoveIn);
        ]
        [ Node.text "In" ];
    ]
