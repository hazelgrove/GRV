module Vdom = Virtual_dom.Vdom
open Vdom.Node
open Vdom.Attr

type 'a t = inject:(Action.t -> Vdom.Event.t) -> Editor.t -> 'a

let mk (widget : 'a t) ~(inject : Action.t -> Vdom.Event.t) (editor : Editor.t)
    : 'a =
  widget ~inject editor

let chars (str : string) : Vdom.Node.t =
  Vdom.Node.span [ Vdom.Attr.class_ "chars" ] [ Vdom.Node.text str ]

let errs (str : string) : Vdom.Node.t =
  Vdom.Node.span [ Vdom.Attr.class_ "errs" ] [ Vdom.Node.text str ]

let maybe_disabled ?(disable : bool = false)
    (node : Vdom.Attr.t List.t -> Vdom.Node.t List.t -> Vdom.Node.t)
    (attrs : Vdom.Attr.t List.t) (children : Vdom.Node.t List.t) : Vdom.Node.t =
  node (attrs @ if disable then [ disabled ] else []) children

let text_input ?(disable : bool = false) (id_ : string)
    (change : string -> Action.t' Option.t) : Vdom.Node.t t =
 fun ~inject editor ->
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
                  Js.focus_editor editor.id;
                  inject { editor_id = editor.id; action } ));
    ]
    []

let button ?(disable : bool = false) (label : string)
    (click : unit -> Action.t' Option.t) : Vdom.Node.t t =
 fun ~inject editor ->
  maybe_disabled ~disable Vdom.Node.button
    [
      on_click (fun _ ->
          match click () with
          | None -> Vdom.Event.Ignore
          | Some action ->
              Js.focus_editor editor.id;
              inject { editor_id = editor.id; action });
    ]
    [ text label ]

let input_button (label : string) (id_ : string) (sort : Lang.Sort.t)
    (mk : string -> Lang.Constructor.t) : Vdom.Node.t t =
 fun ~inject editor ->
  let disable = not (Lang.Index.child_sort editor.cursor.index = sort) in
  let btn : Vdom.Node.t t =
    button ~disable label (fun () ->
        match Js.get_input id_ with
        | "" -> None
        | str -> Some (Edit (Create (mk str))))
  in
  let txt : Vdom.Node.t t =
    text_input ~disable id_ (function
      | "" -> None
      | str -> Some (Edit (Create (mk str))))
  in
  Js.set_input id_ "";
  div [] [ btn ~inject editor; txt ~inject editor ]

let app_button ?(disable : bool = false) (label : string) (action : Action.t') :
    Vdom.Node.t t =
  button ~disable label (fun () -> Some action)

let create_button (label : string) (ctor : Lang.Constructor.t)
    (sort : Lang.Sort.t) : Vdom.Node.t t =
 fun ~inject editor ->
  let disable = not (Lang.Index.child_sort editor.cursor.index = sort) in
  app_button ~disable label (Edit (Create ctor)) ~inject editor

let move_button (label : string) (dir : Action.move) : Vdom.Node.t t =
  app_button label (Move dir)

let select ?(multi : bool = true) ?(default : bool = true) (id_ : string)
    (label : string) (items : 'a List.t) (show : 'a -> Vdom.Node.t) :
    Vdom.Node.t t =
 fun ~inject:(_ : _) _editor ->
  let select_item (k : int) (item : 'a) : Vdom.Node.t =
    div
      [
        ( if default then classes [ "selectItem"; "selected" ]
        else class_ "selectItem" );
        on_click (fun _ ->
            Js.eval_to_unit
              (Printf.sprintf
                 ( if multi then "toggleItem('%s', %d)"
                 else "selectItem('%s', %d)" )
                 id_ k);
            Vdom.Event.Ignore);
      ]
      [ show item ]
  in
  let items = List.mapi select_item items in
  div
    [ classes [ "select"; label ] ]
    [ h2 [] [ text label ]; div [ id id_; class_ "selectItems" ] items ]
