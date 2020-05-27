module Vdom = Virtual_dom.Vdom
open Vdom.Node
open Vdom.Attr

type 'a t = inject:(Action.t -> Vdom.Event.t) -> Model.Instance.t -> 'a

let mk (widget : 'a t) ~(inject : Action.t -> Vdom.Event.t)
    (this_model : Model.Instance.t) : 'a =
  widget ~inject this_model

let chars (str : string) : Vdom.Node.t =
  Vdom.Node.span [ Vdom.Attr.class_ "chars" ] [ Vdom.Node.text str ]

let errs (str : string) : Vdom.Node.t =
  Vdom.Node.span [ Vdom.Attr.class_ "errs" ] [ Vdom.Node.text str ]

let maybe_disabled ?(disable : bool = false)
    (node : Vdom.Attr.t List.t -> Vdom.Node.t List.t -> Vdom.Node.t)
    (attrs : Vdom.Attr.t List.t) (children : Vdom.Node.t List.t) : Vdom.Node.t =
  node (attrs @ if disable then [ disabled ] else []) children

let text_input ?(disable : bool = false) (id_ : string)
    (change : string -> Action.app Option.t) : Vdom.Node.t t =
 fun ~inject this_model ->
  maybe_disabled ~disable Vdom.Node.input
    [
      id id_;
      type_ "text";
      on_change (fun _ _ ->
          match Js.focus_input id_ with
          | "" -> Vdom.Event.Ignore
          | str -> (
              match change str with
              | None -> Vdom.Event.Ignore
              | Some action ->
                  Js.focus_instance this_model.id;
                  inject { instance_id = this_model.id; action } ));
    ]
    []

let button ?(disable : bool = false) (label : string)
    (click : unit -> Action.app Option.t) : Vdom.Node.t t =
 fun ~inject this_model ->
  maybe_disabled ~disable Vdom.Node.button
    [
      on_click (fun _ ->
          match click () with
          | None -> Vdom.Event.Ignore
          | Some action ->
              Js.focus_instance this_model.id;
              inject { instance_id = this_model.id; action });
    ]
    [ text label ]

let input_button (label : string) (id_ : string) (sort : Lang.Sort.t)
    (mk : string -> Lang.Constructor.t) : Vdom.Node.t t =
 fun ~inject this_model ->
  let disable = not (Lang.Index.child_sort this_model.cursor.index = sort) in
  let btn : Vdom.Node.t t =
    button ~disable label (fun () ->
        match Js.get_input id_ with
        | "" -> None
        | str -> Some (Enqueue (Edit (Create (mk str)))))
  in
  let txt : Vdom.Node.t t =
    text_input ~disable id_ (function
      | "" -> None
      | str -> Some (Enqueue (Edit (Create (mk str)))))
  in
  Js.set_input id_ "";
  div [] [ btn ~inject this_model; txt ~inject this_model ]

let app_button ?(disable : bool = false) (label : string) (action : Action.app)
    : Vdom.Node.t t =
  button ~disable label (fun () -> Some action)

let create_button (label : string) (ctor : Lang.Constructor.t)
    (sort : Lang.Sort.t) : Vdom.Node.t t =
 fun ~inject this_model ->
  let disable = not (Lang.Index.child_sort this_model.cursor.index = sort) in
  app_button ~disable label (Enqueue (Edit (Create ctor))) ~inject this_model

let move_button (label : string) (dir : Action.direction) : Vdom.Node.t t =
  app_button label (Enqueue (Move dir))

let select (id_ : string) (label : string) (items : 'a List.t)
    (show : 'a -> Vdom.Node.t) : Vdom.Node.t t =
 fun ~inject:(_ : _) _this_model ->
  let select_item (k : int) (item : 'a) : Vdom.Node.t =
    div
      [
        classes [ "selectItem"; "selected" ];
        on_click (fun _ ->
            Js.eval_to_unit (Printf.sprintf "toggleItem('%s', %d)" id_ k);
            Vdom.Event.Ignore);
      ]
      [ show item ]
  in
  let items = List.mapi select_item items in
  div [ class_ "select" ]
    [ h2 [] [ text label ]; div [ id id_; class_ "selectItems" ] items ]
