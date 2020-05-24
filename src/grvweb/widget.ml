module Vdom = Virtual_dom.Vdom
open Vdom.Node
open Vdom.Attr

type 'a t = inject:(Action.t -> Vdom.Event.t) -> Model.Instance.t -> 'a

let mk (widget : 'a t) ~(inject : Action.t -> Vdom.Event.t)
    (this_model : Model.Instance.t) : 'a =
  widget ~inject this_model

let button ?(disable : bool = false) (label : string) (action : Action.app) :
    Vdom.Node.t t =
 fun ~inject this_model ->
  ( let attrs =
      [
        on_click (fun _ ->
            Js.focus_instance this_model.id;
            inject { instance_id = this_model.id; action });
      ]
    in
    Vdom.Node.button
      (attrs @ if disable then [ disabled ] else [])
      [ text label ]
    : Vdom.Node.t )

let create_button (label : string) (ctor : Lang.Constructor.t)
    (sort : Lang.Sort.t) : Vdom.Node.t t =
 fun ~inject this_model ->
  let disable = not (Lang.Index.child_sort this_model.cursor.index = sort) in
  button ~disable label (Enqueue (Edit (Create ctor))) ~inject this_model

let move_button (label : string) (dir : Action.direction) : Vdom.Node.t t =
 fun ~inject this_model ->
  let action : Action.app = Enqueue (Move dir) in
  Vdom.Node.button
    [ on_click (fun _ -> inject { instance_id = this_model.id; action }) ]
    [ text label ]

let input_button (label : string) (id_ : string) (sort : Lang.Sort.t)
    (mk : string -> Lang.Constructor.t) (unmk : Lang.Constructor.t -> string) :
    Vdom.Node.t t =
 fun ~inject this_model ->
  let disable = not (Lang.Index.child_sort this_model.cursor.index = sort) in
  let () =
    let children = Cache.children this_model.cursor this_model.graph.cache in
    match Edge.Set.elements children with
    | [ edge ] -> Js.set_input id_ @@ unmk (Uuid.Wrap.unmk @@ Edge.target edge)
    | [] | _ :: _ -> Js.set_input id_ ""
  in
  let btn : Vdom.Node.t =
    let attrs =
      [
        ( on_click @@ fun _ ->
          Js.focus_instance this_model.id;
          inject
            {
              instance_id = this_model.id;
              action = Enqueue (Edit (Create (mk @@ Js.get_input id_)));
            } );
      ]
    in
    Vdom.Node.button
      (attrs @ if disable then [ Vdom.Attr.disabled ] else [])
      [ text label ]
  in
  let txt : Vdom.Node.t =
    let attrs =
      [
        id id_;
        type_ "text";
        on_change (fun _ _ ->
            match Js.focus_input id_ with
            | "" -> Vdom.Event.Ignore
            | str ->
                Js.focus_instance this_model.id;
                inject
                  {
                    instance_id = this_model.id;
                    action = Enqueue (Edit (Create (mk str)));
                  });
      ]
    in
    input (attrs @ if disable then [ Vdom.Attr.disabled ] else []) []
  in
  div [] [ btn; txt ]
