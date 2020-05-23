module Vdom = Virtual_dom.Vdom
open Vdom.Node
open Vdom.Attr

type t = inject:(Action.t -> Vdom.Event.t) -> Model.Instance.t -> Vdom.Node.t

let mk (widget : t) ~(inject : Action.t -> Vdom.Event.t)
    (this_model : Model.Instance.t) : Vdom.Node.t =
  widget ~inject this_model

let button ?(disable : bool = false) (label : string) (action : Action.app) : t
    =
 fun ~(inject : Action.t -> Vdom.Event.t) (this_model : Model.Instance.t) ->
  ( let attrs =
      [
        on_click (fun _ ->
            Js.eval_to_unit
              ("refocus('instance" ^ Int.to_string this_model.id ^ "')");
            inject { instance_id = this_model.id; action });
      ]
    in
    Vdom.Node.button
      (attrs @ if disable then [ disabled ] else [])
      [ text label ]
    : Vdom.Node.t )

let create_button (label : string) (ctor : Lang.Constructor.t)
    (sort : Lang.Sort.t) : t =
 fun ~(inject : Action.t -> Vdom.Event.t) (this_model : Model.Instance.t) ->
  ( let disable = not (Lang.Index.child_sort this_model.cursor.index = sort) in
    button ~disable label (Enqueue (Edit (Create ctor))) ~inject this_model
    : Vdom.Node.t )

let move_button (label : string) (dir : Action.direction) : t =
 fun ~(inject : Action.t -> Vdom.Event.t) (this_model : Model.Instance.t) ->
  ( let action : Action.app = Enqueue (Move dir) in
    Vdom.Node.button
      [ on_click (fun _ -> inject { instance_id = this_model.id; action }) ]
      [ text label ]
    : Vdom.Node.t )

let input_button (label : string) (id_ : string) (sort : Lang.Sort.t)
    (mk : string -> Lang.Constructor.t)
    (unmk : Lang.Constructor.t -> string Option.t) : t =
 fun ~(inject : Action.t -> Vdom.Event.t) (this_model : Model.Instance.t) ->
  let disable = not (Lang.Index.child_sort this_model.cursor.index = sort) in
  let () =
    let children = Cache.children this_model.cursor this_model.graph.cache in
    match Edge.Set.elements children with
    | [ edge ] -> (
        match unmk (Uuid.Wrap.unmk @@ Edge.target edge) with
        | Some str ->
            Js.eval_to_unit
            @@ Printf.sprintf "setInput('%s', '%s')" id_
            @@ String.escaped str
        | None -> Js.eval_to_unit @@ "setInput('" ^ id_ ^ "', '')" )
    | [] | _ :: _ -> Js.eval_to_unit @@ "setInput('" ^ id_ ^ "', '')"
  in
  let btn : Vdom.Node.t =
    let attrs =
      [
        ( on_click @@ fun _ ->
          let pat = Js.eval_to_string @@ "getInput('" ^ id_ ^ "')" in
          Js.eval_to_unit
            ("refocus('instance" ^ Int.to_string this_model.id ^ "')");
          inject
            {
              instance_id = this_model.id;
              action = Enqueue (Edit (Create (mk pat)));
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
            match Key.focus_input id_ this_model with
            | Some str ->
                inject
                  {
                    instance_id = this_model.id;
                    action = Enqueue (Edit (Create (mk str)));
                  }
            | None -> Vdom.Event.Ignore);
      ]
    in
    input (attrs @ if disable then [ Vdom.Attr.disabled ] else []) []
  in
  div [] [ btn; txt ]
