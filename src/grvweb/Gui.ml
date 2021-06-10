module Vdom = Virtual_dom.Vdom
module Node = Virtual_dom.Vdom.Node
module Attr = Virtual_dom.Vdom.Attr

type context = { inject : Action.t -> Vdom.Event.t; editor : Editor.t }

let clicks_to (ctx : context) (cursor : Cursor.t) : Attr.t =
  Attr.on_click (fun event ->
      Js.claim_event event;
      ctx.inject
        { Action.editor_id = ctx.editor.id; action = Move (Select cursor) })

let selected (selection : bool list) (elements : 'a list) : 'a list =
  List.combine selection elements |> List.filter fst |> List.map snd

let send (model : Model.t) (editor : Editor.t) : Action.t' Option.t =
  match Js.get_selection ("actions" ^ Uuid.Id.to_string editor.id) with
  | [] -> None
  | selection ->
      let actions : Graph_action.t list =
        Graph_action.Set.elements editor.actions |> selected selection
      in
      let editor_ids : Uuid.Id.t list =
        Uuid.Map.bindings model.editors
        |> List.rev_map snd
        |> selected (Js.get_selection ("editors" ^ Uuid.Id.to_string editor.id))
        |> List.map (fun (editor : Editor.t) -> editor.id)
      in
      Js.fill_selection ("actions" ^ Uuid.Id.to_string editor.id);
      Some (Comm (Send (actions, editor_ids)))

let restore (editor : Editor.t) (deleted : Old_Vertex.Set.t)
    (vertex_id : string) : Action.t' Option.t =
  let%map.Util.Option selection : Old_Vertex.t option =
    if String.equal vertex_id "" then
      let selection : bool list =
        Js.get_selection ("deleted" ^ Uuid.Id.to_string editor.id)
      in
      let vertexes : Old_Vertex.t list = Old_Vertex.Set.elements deleted in
      List.nth_opt (selected selection vertexes) 0
    else Old_Graph.vertex editor.graph (Uuid.Id.of_string vertex_id)
  in
  Action.Edit (Restore selection)

let base_attrs ~(classes : string list) ?(disabled : bool = false)
    (id : string option) : Attr.t list =
  (match id with None -> [] | Some id -> [ Attr.id id ])
  @ [ Attr.classes classes ]
  @ if disabled then [ Attr.disabled ] else []

let apply_action (ctx : context) (action : Action.t' option)
    (tabindexes : int Uuid.Map.t) : Vdom.Event.t =
  match action with
  | None -> Vdom.Event.Ignore
  | Some action ->
      Js.focus
        ("editor" ^ Int.to_string (Uuid.Map.find ctx.editor.id tabindexes));
      ctx.inject { editor_id = ctx.editor.id; action }

let text_input ?(classes : string list = []) ?(disabled : bool = false)
    ~(on_change : string -> Action.t' option) (ctx : context) (id : string)
    (tabindexes : int Uuid.Map.t) : Node.t =
  Js.set_input id "";
  Node.input
    (base_attrs (Some id) ~classes ~disabled
    @ [
        Attr.on_change (fun _ (value : string) ->
            match value with
            | "" -> Vdom.Event.Ignore
            | str -> apply_action ctx (on_change str) tabindexes);
      ])
    []

let button ?(classes : string list = []) ?(disabled : bool = false)
    ~(on_click : unit -> Action.t' option) (ctx : context) (label : string)
    (tabindexes : int Uuid.Map.t) : Node.t =
  Node.button
    (base_attrs None ~classes ~disabled
    @ [ Attr.on_click (fun _ -> apply_action ctx (on_click ()) tabindexes) ])
    [ Node.text label ]

let none_button ~(on_click : unit -> unit) (ctx : context) (label : string)
    (tabindexes : int Uuid.Map.t) : Node.t =
  button ctx label tabindexes ~on_click:(fun () ->
      on_click ();
      None)

let some_button ?(disabled : bool = false) (ctx : context) (label : string)
    (tabindexes : int Uuid.Map.t) (action : Action.t') : Node.t =
  button ctx label tabindexes ~disabled ~on_click:(fun () -> Some action)

let button_text_input ?(classes : string list = []) ?(disabled : bool option)
    ~(on_click : unit -> Action.t' option)
    ~(on_change : string -> Action.t' option) (ctx : context) (label : string)
    (id : string) (tabindexes : int Uuid.Map.t) : Node.t =
  let disabled = Option.value disabled ~default:false in
  Node.div
    (base_attrs (Some id) ~classes ~disabled)
    [
      button ctx label tabindexes ~disabled ~on_click;
      text_input ctx id tabindexes ~disabled ~on_change;
    ]

let sorted_text_input ?(classes : string list = [])
    ~(on_change : string -> Action.t' option) (ctx : context) (id : string)
    (sort : Lang.Sort.t) (tabindexes : int Uuid.Map.t) : Node.t =
  let disabled =
    not (Lang.Position.child_sort ctx.editor.cursor.position = sort)
  in
  text_input ctx id tabindexes ~classes ~disabled ~on_change

let sorted_button ?(classes : string list = [])
    ~(on_click : unit -> Action.t' option) (ctx : context) (label : string)
    (sort : Lang.Sort.t) (tabindexes : int Uuid.Map.t) : Node.t =
  let disabled =
    not (Lang.Position.child_sort ctx.editor.cursor.position = sort)
  in
  button ctx label tabindexes ~classes ~disabled ~on_click

let action_button (ctx : context) (label : string) (sort : Lang.Sort.t)
    (tabindexes : int Uuid.Map.t) (id_opt : string option)
    (mk_action : string -> Action.t') : Node.t =
  sorted_button ctx label sort tabindexes ~on_click:(fun () ->
      match Option.map Js.prompt id_opt with
      | None -> Some (mk_action "")
      | Some "" -> None
      | Some str ->
          Js.focus ("editor" ^ Option.get id_opt);
          Some (mk_action str))

let select ?(classes : string list = []) ?(multi : bool = true)
    ?(default : bool = multi) ?(label : string option) (id : string)
    (items : 'a list) (view_item : 'a -> Node.t list) : Node.t =
  let classes : string list =
    classes @ [ "selectItem" ] @ if default then [ "selected" ] else []
  in
  let select_item (i : int) (item : 'a) : Node.t =
    Node.div
      (base_attrs None ~classes ~disabled:false
      @ [
          Attr.on_click (fun _ ->
              if multi then Js.toggle_item id i else Js.select_item id i;
              Vdom.Event.Ignore);
        ])
      (view_item item)
  in
  let classes, heading =
    match label with
    | None -> (classes, [])
    | Some label when String.contains label ' ' ->
        (classes, [ Node.h1 [] [ Node.text label ] ])
    | Some label -> (classes @ [ label ], [ Node.h1 [] [ Node.text label ] ])
  in
  Node.div
    [ Attr.classes (classes @ [ "select" ]) ]
    (heading
    @ [
        Node.div
          [ Attr.id id; Attr.class_ "selectItems" ]
          (List.mapi select_item items);
      ])

let teleport (ctx : context) (id : string) : unit -> Action.t' option =
 fun () ->
  match Js.prompt "edge_id or vertex_id" with
  | "" -> None
  | str -> (
      let teleport_id = Uuid.Id.of_string str in
      Js.focus ("editor" ^ id);
      match
        Old_Edge.Set.find_first_opt
          (fun edge -> edge.id = teleport_id)
          (Old_Graph.edges ctx.editor.graph)
      with
      | Some edge -> Some (Move (Select edge.value.source))
      | None -> (
          match
            Old_Vertex.Set.find_first_opt
              (fun vertex -> vertex.id = teleport_id)
              (Old_Graph.vertexes ctx.editor.graph)
          with
          | None -> None
          | Some vertex -> (
              match
                Old_Edge.Set.choose_opt
                  (Old_Graph.parent_edges ctx.editor.graph vertex)
              with
              | Some edge -> Some (Move (Select edge.value.source))
              | None -> None)))

let break : Node.t = Node.div [ Attr.class_ "break" ] []

let panel ?(classes : string list = []) ?(label : string option)
    (nodes : Node.t list) : Node.t =
  let heading =
    match label with
    | Some label -> [ Node.h1 [] [ Node.text label ] ]
    | None -> []
  in
  Node.div
    (base_attrs None ~classes:(classes @ [ "panel" ]))
    (heading @ [ break ] @ nodes)

let select_panel ?(classes : string list = []) ?(multi : bool = true)
    ?(default : bool = multi) ?(label : string option) (id : string)
    (items : 'a list) (view_item : 'a -> Node.t list) (nodes : Node.t list) :
    Node.t =
  let selector =
    match label with
    | None -> select id items view_item ~multi ~default
    | Some label when String.contains label ' ' ->
        select id items view_item ~multi ~default ~label ~classes
    | Some label -> select id items view_item ~multi ~default ~label
  in
  Node.div
    [ Attr.classes (classes @ [ "selector" ]) ]
    ([ selector ] @ [ break ] @ nodes)
