module Vdom = Virtual_dom.Vdom

let view ~(inject : Action.t -> Vdom.Event.t) (model : Model.t) =
  let open Vdom in
  Node.div []
    [
      Node.text (Printf.sprintf "%d" model.counter);
      Node.br [];
      Node.button
        [
          Attr.on_click
            (fun (_ : Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t) ->
              inject Action.Increment);
        ]
        [ Node.text "Increment" ];
    ]
