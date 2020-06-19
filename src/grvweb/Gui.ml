module Dom_html = Js_of_ocaml.Dom_html
module Node = Virtual_dom.Vdom.Node
module Attr = Virtual_dom.Vdom.Attr
module Event = Virtual_dom.Vdom.Event

let send (model : Model.t) (editor : Editor.t) : Action.t' Option.t =
  match Js.get_selection ("actions" ^ Uuid.Id.show editor.id) with
  | [] -> None
  | selection ->
      let actions =
        List.(
          map snd
            (filter fst
               (combine selection (Graph_action.Set.elements editor.actions))))
      in
      let editor_ids : Uuid.Id.t list =
        let selection = Js.get_selection ("editors" ^ Uuid.Id.show editor.id) in
        let editors = List.rev_map snd (Uuid.Map.bindings model.editors) in
        List.(
          map
            (fun (editor : Editor.t) -> editor.id)
            (map snd (filter fst (combine selection editors))))
      in
      Js.fill_selection ("actions" ^ Uuid.Id.show editor.id);
      Some (Comm (Send (actions, editor_ids)))

let restore (editor : Editor.t) (vertex_id : string) : Action.t' Option.t =
  let%map.Util.Option selection : Vertex.t option =
    if String.equal vertex_id "" then
      let selection = Js.get_selection ("deleted" ^ Uuid.Id.show editor.id) in
      let vertexes : Vertex.t list =
        Vertex.Set.elements (Graph.roots editor.graph).deleted
      in
      let%map.Util.Option result : (bool * Vertex.t) Option.t =
        List.find_opt fst (List.combine selection vertexes)
      in
      snd result
    else Graph.vertex editor.graph (Uuid.Id.read vertex_id)
  in
  Action.Edit (Restore selection)

let base_attrs ?(id : string option) ~(classes : string list)
    ?(disabled : bool = false) () : Attr.t list =
  (match id with None -> [] | Some id -> [ Attr.id id ])
  @ [ Attr.classes classes ]
  @ if disabled then [ Attr.disabled ] else []

let apply_action (action : Action.t' option) (inject : Action.t -> Event.t)
    (editor : Editor.t) : Event.t =
  match action with
  | None -> Event.Ignore
  | Some action ->
      Js.focus_editor editor.id;
      inject { editor_id = editor.id; action }

let text_input ?(classes : string list = []) ?(disabled : bool = false)
    ~(on_change : string -> Action.t' option) (id : string)
    (inject : Action.t -> Event.t) (editor : Editor.t) : Node.t =
  Js.set_input id "";
  Node.input
    ( base_attrs ~id ~classes ~disabled ()
    @ [
        Attr.on_change (fun _ (value : string) ->
            match value with
            | "" -> Event.Ignore
            | str -> apply_action (on_change str) inject editor);
      ] )
    []

let button ?(classes : string list = []) ?(disabled : bool = false)
    ~(on_click : unit -> Action.t' option) (label : string)
    (inject : Action.t -> Event.t) (editor : Editor.t) : Node.t =
  Node.button
    ( base_attrs ~classes ~disabled ()
    @ [ Attr.on_click (fun _ -> apply_action (on_click ()) inject editor) ] )
    [ Node.text label ]

let button_text_input ?(classes : string list = []) ?(disabled : bool option)
    ~(on_click : unit -> Action.t' option)
    ~(on_change : string -> Action.t' option) (label : string) (id : string)
    (inject : Action.t -> Event.t) (editor : Editor.t) : Node.t =
  let disabled = Option.value disabled ~default:false in
  Node.div
    (base_attrs ~id ~classes ~disabled ())
    [
      button label inject editor ~disabled ~on_click;
      text_input id inject editor ~disabled ~on_change;
    ]

let sorted_text_input ?(classes : string list = [])
    ~(on_change : string -> Action.t' option) (id : string) (sort : Lang.Sort.t)
    (inject : Action.t -> Event.t) (editor : Editor.t) : Node.t =
  let disabled = not (Lang.Index.child_sort editor.cursor.index = sort) in
  text_input id inject editor ~classes ~disabled ~on_change

let sorted_button ?(classes : string list = [])
    ~(on_click : unit -> Action.t' option) (label : string) (sort : Lang.Sort.t)
    (inject : Action.t -> Event.t) (editor : Editor.t) : Node.t =
  let disabled = not (Lang.Index.child_sort editor.cursor.index = sort) in
  button label inject editor ~classes ~disabled ~on_click

let select ?(classes : string list = []) ?(multi : bool = true)
    ?(default : bool = multi) ?(label : string option) (id : string)
    (items : 'a list) (view_item : 'a -> Node.t) : Node.t =
  let select_item (i : int) (item : 'a) : Node.t =
    Node.div
      ( base_attrs
          ~classes:([ "selectItem" ] @ if default then [ "selected" ] else [])
          ~disabled:false ()
      @ [
          Attr.on_click (fun _ ->
              if multi then Js.toggle_item id i else Js.select_item id i;
              Event.Ignore);
        ] )
      [ view_item item ]
  in
  let classes, heading =
    match label with
    | None -> (classes, [])
    | Some label -> (classes @ [ label ], [ Node.h1 [] [ Node.text label ] ])
  in
  Node.div
    [ Attr.classes (classes @ [ "select" ]) ]
    ( heading
    @ [
        Node.div
          [ Attr.id id; Attr.class_ "selectItems" ]
          (List.mapi select_item items);
      ] )

let break : Node.t = Node.div [ Attr.class_ "break" ] []

let panel ?(classes : string list = []) ?(label : string option)
    (nodes : Node.t list) : Node.t =
  let heading =
    match label with
    | Some label -> [ Node.h1 [] [ Node.text label ] ]
    | None -> []
  in
  Node.div
    (base_attrs ~classes:(classes @ [ "panel" ]) ())
    (heading @ [ break ] @ nodes)

let select_panel ?(classes : string list = []) ?(multi : bool = true)
    ?(default : bool = multi) ?(label : string option) (id : string)
    (items : 'a list) (view_item : 'a -> Node.t) (nodes : Node.t list) : Node.t
    =
  let selector =
    match label with
    | None -> select ~multi ~default id items view_item
    | Some label -> select ~multi ~default ~label id items view_item
  in
  Node.div
    [ Attr.classes (classes @ [ "selector" ]) ]
    ([ selector ] @ [ break ] @ nodes)
