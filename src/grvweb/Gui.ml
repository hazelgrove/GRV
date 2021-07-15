(* open OptionUtil.Syntax *)
module Vdom = Virtual_dom.Vdom
module Node = Virtual_dom.Vdom.Node
module Attr = Virtual_dom.Vdom.Attr

type context = { inject : Action.t -> Vdom.Event.t; editor : Editor.t }

let clicks_to (ctx : context) (vertex_id : Vertex.id) : Attr.t =
  Attr.on_click (fun event ->
      Js.claim_event event;
      let editor_id = ctx.editor.id in
      let action = Action.(UserAction (Select vertex_id)) in
      ctx.inject { editor_id; action })

let selected (selection : bool list) (elements : 'a list) : 'a list =
  List.combine selection elements |> List.filter fst |> List.map snd

let send (model : Model.t) (editor : Editor.t) : Action.t' option =
  let editor_id = Id.to_string editor.id in
  match Js.get_selection ("actions" ^ editor_id) with
  | [] -> None
  | selection ->
      let actions =
        GraphAction.Set.elements editor.local_actions |> selected selection
      in
      let editor_ids =
        Id.Map.bindings model.editors
        |> List.rev_map snd
        |> selected (Js.get_selection ("editors" ^ editor_id))
        |> List.map (fun (editor : Editor.t) -> editor.id)
      in
      Js.fill_selection ("actions" ^ editor_id);
      Some Action.(EditorAction (Send (actions, editor_ids)))

(* let restore (editor : Editor.t) (deleted : Vertex.Set.t)
    (vertex_id_opt : Vertex.id option) : Action.t' option =
  let editor_id = Id.to_string editor.id in
  let+ selection =
    match vertex_id_opt with
    | None ->
        let selection = Js.get_selection ("deleted" ^ editor_id) in
        let vertexes = Vertex.Set.elements deleted in
        List.nth_opt (selected selection vertexes) 0
    | Some vertex_id -> ZGrove.find_vertex vertex_id editor.zgrove
  in
  Action.Edit (Restore selection) *)

let base_attrs ~(classes : string list) ?(disabled : bool = false)
    (id_opt : string option) : Attr.t list =
  let maybe_id = match id_opt with None -> [] | Some id -> [ Attr.id id ] in
  let classes = [ Attr.classes classes ] in
  let maybe_disabled = if disabled then [ Attr.disabled ] else [] in
  maybe_id @ classes @ maybe_disabled

let apply_action (ctx : context) (action : Action.t' option)
    (tabindexes : int Id.Map.t) : Vdom.Event.t =
  match Id.Map.find_opt ctx.editor.id tabindexes with
  | None -> Vdom.Event.Ignore
  | Some tabindex -> (
      let editor_idx = Int.to_string tabindex in
      match action with
      | None -> Vdom.Event.Ignore
      | Some action ->
          Js.focus ("editor" ^ editor_idx);
          ctx.inject { editor_id = ctx.editor.id; action })

let text_input ?(classes : string list = []) ?(disabled : bool = false)
    ~(on_change : string -> Action.t' option) (ctx : context)
    (input_id : string) (tabindexes : int Id.Map.t) : Node.t =
  Js.set_input input_id "";
  let on_change =
    Attr.on_change (fun _ -> function
      | "" -> Vdom.Event.Ignore
      | str -> apply_action ctx (on_change str) tabindexes)
  in
  let attrs = base_attrs (Some input_id) ~classes ~disabled @ [ on_change ] in
  Node.input attrs []

let button ?(classes : string list = []) ?(disabled : bool = false)
    ~(on_click : unit -> Action.t' option) (ctx : context) (label : string)
    (tabindexes : int Id.Map.t) : Node.t =
  let on_click =
    Attr.on_click (fun _ -> apply_action ctx (on_click ()) tabindexes)
  in
  let attrs = base_attrs None ~classes ~disabled @ [ on_click ] in
  Node.button attrs [ Node.text label ]

let none_button ~(on_click : unit -> unit) (ctx : context) (label : string)
    (tabindexes : int Id.Map.t) : Node.t =
  let on_click () =
    on_click ();
    None
  in
  button ctx label tabindexes ~on_click

let some_button ?(disabled : bool = false) (ctx : context) (label : string)
    (tabindexes : int Id.Map.t) (action : Action.t') : Node.t =
  let on_click () = Some action in
  button ctx label tabindexes ~disabled ~on_click

let button_text_input ?(classes : string list = []) ?(disabled : bool option)
    ~(on_click : unit -> Action.t' option)
    ~(on_change : string -> Action.t' option) (ctx : context) (label : string)
    (input_id : string) (tabindexes : int Id.Map.t) : Node.t =
  let disabled = Option.value disabled ~default:false in
  let attrs = base_attrs (Some input_id) ~classes ~disabled in
  let btn = button ctx label tabindexes ~disabled ~on_click in
  let txt = text_input ctx input_id tabindexes ~disabled ~on_change in
  Node.div attrs [ btn; txt ]

let sorted_text_input ?(classes : string list = [])
    ~(on_change : string -> Action.t' option) (ctx : context)
    (input_id : string) (sort : GroveLang.Sort.t) (tabindexes : int Id.Map.t) :
    Node.t =
  let disabled =
    match ZGrove.cursor_position ctx.editor.zgrove with
    | None -> true
    | Some position -> not (GroveLang.Position.sort position = sort)
  in
  text_input ctx input_id tabindexes ~classes ~disabled ~on_change

let sorted_button ?(classes : string list = [])
    ~(on_click : unit -> Action.t' option) (ctx : context) (label : string)
    (sort : GroveLang.Sort.t) (tabindexes : int Id.Map.t) : Node.t =
  let disabled =
    match ZGrove.cursor_position ctx.editor.zgrove with
    | None -> true
    | Some position -> not (GroveLang.Position.sort position = sort)
  in
  button ctx label tabindexes ~classes ~disabled ~on_click

let action_button (ctx : context) (label : string) (sort : GroveLang.Sort.t)
    (tabindexes : int Id.Map.t) (editor_idx_opt : int option)
    (mk_action : string -> Action.t') : Node.t =
  let editor_idx_opt = Option.map Int.to_string editor_idx_opt in
  let on_click () =
    match editor_idx_opt with
    | None -> Some (mk_action "")
    | Some editor_idx -> (
        match Js.prompt editor_idx with
        | "" -> None
        | str ->
            Js.focus ("editor" ^ editor_idx);
            Some (mk_action str))
  in
  sorted_button ctx label sort tabindexes ~on_click

let select ?(classes : string list = []) ?(multi : bool = true)
    ?(default : bool = multi) ?(label : string option) (select_id : string)
    (items : 'a list) (view_item : 'a -> Node.t list) : Node.t =
  let maybe_selected = if default then [ "selected" ] else [] in
  let classes = classes @ [ "selectItem" ] @ maybe_selected in
  let select_item i item =
    let attrs = base_attrs None ~classes ~disabled:false in
    let on_click _ =
      if multi then Js.toggle_item select_id i else Js.select_item select_id i;
      Vdom.Event.Ignore
    in
    Node.div (attrs @ [ Attr.on_click on_click ]) (view_item item)
  in
  let classes =
    match label with
    | None -> classes
    | Some label when String.contains label ' ' -> classes
    | Some label -> classes @ [ label ]
  in
  let attrs = [ Attr.classes (classes @ [ "select" ]) ] in
  let maybe_heading =
    match label with
    | None -> []
    | Some label -> [ Node.h1 [] [ Node.text label ] ]
  in
  let items =
    Node.div
      [ Attr.id select_id; Attr.class_ "selectItems" ]
      (List.mapi select_item items)
  in
  Node.div attrs (maybe_heading @ [ items ])

(* let teleport (ctx : context) (id : string) : unit -> Action.t' option =
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
              | None -> None))) *)

let break : Node.t = Node.div [ Attr.class_ "break" ] []

let panel ?(classes : string list = []) ?(label : string option)
    (nodes : Node.t list) : Node.t =
  let classes = classes @ [ "panel" ] in
  let attrs = base_attrs None ~classes in
  let maybe_heading =
    match label with
    | None -> []
    | Some label -> [ Node.h1 [] [ Node.text label ] ]
  in
  Node.div attrs (maybe_heading @ break :: nodes)

let select_panel ?(classes : string list = []) ?(multi : bool = true)
    ?(default : bool = multi) ?(label : string option) (select_id : string)
    (items : 'a list) (view_item : 'a -> Node.t list) (nodes : Node.t list) :
    Node.t =
  let selector =
    match label with
    | None -> select select_id items view_item ~multi ~default
    | Some label when String.contains label ' ' ->
        select select_id items view_item ~multi ~default ~label ~classes
    | Some label -> select select_id items view_item ~multi ~default ~label
  in
  let attrs = [ Attr.classes (classes @ [ "selector" ]) ] in
  Node.div attrs (selector :: break :: nodes)
