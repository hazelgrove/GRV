open OptionUtil.Syntax
open Sexplib0.Sexp_conv

module ModelAction = struct
  type t = Record | Report | Stop | Replay of string | Dump | Load of string
  [@@deriving sexp]
end

module EditorAction = struct
  type t =
    | Send of GraphAction.t list * Editor.id list
    | Clone of Editor.id
    | Drop of Editor.id
    | ToggleIds of Editor.id
  [@@deriving sexp]
end

module UserAction = struct
  type direction = Up | Down | Left | Right [@@deriving sexp]

  type t =
    | Move of direction
    | Select of Vertex.id
    | Construct of GroveLang.Constructor.t
    | Delete
    | Reposition of Vertex.t * GroveLang.Position.t
  [@@deriving sexp]
end

type t' =
  | ModelAction of ModelAction.t
  | EditorAction of EditorAction.t
  | UserAction of UserAction.t
[@@deriving sexp]

type t = { editor_id : Editor.id; action : t' } [@@deriving sexp]

(* module ActionContext = struct
  type nonrec t = { model : Model.t; editor_id : Editor.id; action : t' }

  let editor_opt (ctx : t) : Editor.t option =
    Id.Map.find_opt ctx.editor_id ctx.model.editors

  let cursor_sort (ctx : t) : GroveLang.Sort.t option =
    let+ editor = editor_opt ctx in
    ZGrove.cursor_sort editor.zgrove

  let cursor_term (ctx : t) : Term.t option =
    let+ editor = editor_opt ctx in
    ZGrove.cursor_term editor.zgrove
end *)

(* let perform_user_action (ctx : ActionContext.t) (user_action : UserAction.t) :  *)

let translate (user_action : UserAction.t) (editor : Editor.t)
    (u_gen : Id.Gen.t) : (GraphAction.Set.t * Id.Gen.t) option =
  match user_action with
  | Move Up -> None
  | Move Down -> None
  | Move Left -> None
  | Move Right -> None
  | Select _ -> None
  | Construct k -> (
      match ZGrove.cursor_term editor.zgrove with
      | Exp (Hole (source, position))
      | Pat (Hole (source, position))
      | Typ (Hole (source, position)) ->
          if GroveLang.is_well_sorted source.constructor position k then
            let target, u_gen = Vertex.mk u_gen k in
            let edge, u_gen = Edge.mk u_gen source position target in
            Some (GraphAction.Set.singleton (Plus, edge), u_gen)
          else ( ?? )
      | _ -> ( ??))
  | Delete -> None
  | Reposition (_, _) -> None

let apply (model : Model.t) (action : t) (_state : State.t)
    ~schedule_action:(_ : t -> unit) : Model.t =
  print_endline "ACTION";
  let { editor_id; action } = action in
  match Id.Map.find_opt editor_id model.editors with
  | None -> model
  | Some editor -> (
      match action with
      | ModelAction _ -> model
      | EditorAction _ -> model
      | UserAction user_action -> (
          match translate user_action editor model.u_gen with
          | None -> model
          | Some (graph_actions, u_gen) ->
              let editor = Editor.apply_graph_actions graph_actions editor in
              let local_actions =
                GraphAction.Set.union graph_actions editor.local_actions
              in
              let editor = { editor with local_actions } in
              let editors = Id.Map.add editor.id editor model.editors in
              { model with editors; u_gen }))
(* (
   match user_action with
   | Move Up -> model
   | Move Down -> model
   | Move Left -> model
   | Move Right -> model
   | Select _ -> model
   | Construct k -> (
       match ActionContext.cursor_term ctx with
       | Exp (Hole (v_s, p_s))
       | Pat (Hole (v_s, p_s))
       | Typ (Hole (v_s, p_s)) ->
           if GroveLang.is_well_sorted k_s p_s k then ( ?? ) else ( ?? ))
   | Delete -> model
   | Reposition (_, _) -> model)) *)

(*

ModelAction: top-level / global actions in the Grove UI (stuff that works on the incr_dom model), e.g., EditorAction
EditorAction: per-editor actions; something that operates on a particular editor (editor_id), e.g., clone
UserAction: things that operate on the grove and potentially emits graph edits
Comms: Send

perform_user_action : ZGrove -> UserAction -> ZGrove
*)

(* let apply (model : Model.t) (action : t) (_state : State.t)
    ~schedule_action:(_ : t -> unit) : Model.t =
  Option.value
    (match action.action with
    | Move move_action -> Model.update_editor (Editor.move move_action) model
    | Edit edit_action -> Model.update_editor (Editor.edit edit_action) model
    | Send (graph_actions, editor_ids) ->
        let* editors = Model.get_editors editor_ids model in
        Model.update_editors (Editor.send graph_actions) editors model)
    ~default:model *)

(* Option.value (let* editor = Id.Map.find_opt action.editor_id model.editors in
   let update =
     match action.action with
     | Move move_action ->
       let* editor = Editor.move move_action editor in
       Model.update_editor
     | Edit edit_action -> Editor.edit edit_action editor
     | Send (graph_actions, editor_ids) ->
       let* editors = List.map (fun editor_id -> Id.Map.find_opt editor_id model.editors) |> OptionUtil.of_list in
       ()
       (* let* editor = Id.Map.find_opt Editor.send send_action *)
     | Env env_action -> apply_env env_action
   in
   Option.value (update model) ~default:model *)

(* let apply_graph_action (graph_action : GraphAction.t) (editor : Editor.t) :
    Editor.t =
  let graph = Old_Graph.apply_action editor.graph graph_action in
  let known_actions = Graph_action.Set.add graph_action editor.known_actions in
  let actions = Graph_action.Set.add graph_action editor.actions in
  { editor with graph; known_actions; actions } *)

(* let record_actions (model : Model.t) (editor_id : Editor.id)
    (graph_actions : GraphAction.t list) : Model.graph_actions option =
  let+ actions = model.actions in
  let new_actions = List.map (fun x -> (editor_id, x)) graph_actions in
  new_actions @ actions *)

(* let report_actions (actions : Model.graph_actions) : unit =
  Model.sexp_of_graph_actions actions |> Util.Sexp.print *)

(* let replay_actions (model : Model.t) (actions : Model.graph_actions) :
    Model.t option =
  List.fold_left
    (fun model_opt (editor_id, graph_action) ->
      let* (model : Model.t option) = model_opt in
      let* editor = Id.Map.find_opt editor_id model.editors in
      let editor = apply_graph_action graph_action editor in
      let editors = Uuid.Map.add editor_id editor model.editors in
      let actions = record_actions model editor_id [ graph_action ] in
      Some Model.{ editors; actions })
    (Some model) actions *)

(* let apply_move (model : Model.t) (editor_id : Uuid.Id.t) (move_action : move) :
    Model.t option =
  let%bind.Util.Option editor = Uuid.Map.find_opt editor_id model.editors in
  let cursor : Cursor.t = editor.cursor in
  let cursor : Cursor.t option =
    match move_action with
    | Left ->
        let%map.Util.Option position =
          Lang.Position.left editor.cursor.position
        in
        { cursor with position }
    | Right ->
        let%map.Util.Option position =
          Lang.Position.right editor.cursor.position
        in
        { cursor with position }
    | Up -> (
        match
          Old_Graph.parent_edges editor.graph editor.cursor.vertex
          |> Old_Edge.Set.elements
        with
        | [ edge ] -> Some edge.value.source
        | _ -> None)
    | Down -> (
        match
          Old_Graph.child_edges editor.graph cursor.vertex cursor.position
          |> Old_Edge.Set.elements
        with
        | [ edge ] ->
            let vertex = edge.value.target in
            let%map.Util.Option position = Lang.Position.down vertex.value in
            { Cursor.vertex; position }
            (* TODO: how to choose between ambiguous children *)
        | _ -> None)
    | Select cursor -> Some cursor
  in
  let%map.Util.Option cursor = cursor in
  let editors = Uuid.Map.add editor_id { editor with cursor } model.editors in
  { model with editors } *)

(* let apply_edit (model : Model.t) (editor_id : Uuid.Id.t) (edit_action : edit) :
    Model.t option =
  let%bind.Util.Option editor = Uuid.Map.find_opt editor_id model.editors in
  let children =
    Old_Graph.child_edges editor.graph editor.cursor.vertex
      editor.cursor.position
  in
  let move_in, graph_actions =
    match edit_action with
    | Create constructor -> (
        if
          not
            (Lang.Position.child_sort editor.cursor.position
            = Lang.Constructor.sort_of constructor)
        then (false, [])
        else
          let vertex = Old_Vertex.mk constructor in
          let create_parent_edge =
            [
              Graph_action.
                { state = Created; edge = Old_Edge.mk editor.cursor vertex };
            ]
          in
          match Lang.Position.default_position constructor with
          | None -> (false, create_parent_edge)
          | Some position ->
              let create_new_children_edges =
                Old_Edge.Set.elements children
                |> List.map (fun (edge : Old_Edge.t) ->
                       let source = Cursor.{ vertex; position } in
                       let edge = Old_Edge.mk source edge.value.target in
                       Graph_action.{ state = Created; edge })
              in

              let destroy_old_children_edges =
                Old_Edge.Set.elements children
                |> List.map (fun edge -> Graph_action.{ state = Deleted; edge })
              in
              ( true,
                create_parent_edge @ create_new_children_edges
                @ destroy_old_children_edges ))
    | Destroy ->
        ( false,
          Old_Graph.child_edges editor.graph editor.cursor.vertex
            editor.cursor.position
          |> Old_Edge.Set.elements
          |> List.map (fun edge -> Graph_action.{ state = Deleted; edge }) )
    | Restore vertex ->
        let edge : Old_Edge.t = Old_Edge.mk editor.cursor vertex in
        (false, [ Graph_action.{ state = Created; edge } ])
    | DropEdge edge_id -> (
        ( false,
          match
            Old_Graph.edges editor.graph
            |> Old_Edge.Set.find_first_opt (fun edge -> edge.id = edge_id)
          with
          | Some edge -> [ Graph_action.{ state = Deleted; edge } ]
          | None -> [] ))
  in
  let editor = List.fold_right apply_graph_action graph_actions editor in
  let editors = Uuid.Map.add editor_id editor model.editors in
  let actions = record_actions model editor.id graph_actions in
  let model = Model.{ editors; actions } in
  if move_in then apply_move model editor_id Down else Some model *)

(* let apply_comm (model : Model.t) (editor_id : Uuid.Id.t) (comm_action : comm) :
    Model.t option =
  match comm_action with
  | Send (edit_actions, editor_ids) ->
      let%map.Util.Option editors =
        List.fold_left
          (fun editors_opt editor_id ->
            let%bind.Util.Option editors = editors_opt in
            let%map.Util.Option editor =
              Uuid.Map.find_opt editor_id model.editors
            in
            editor :: editors)
          (Some []) editor_ids
      in
      let model =
        List.fold_left
          (fun (model : Model.t) (editor : Editor.t) ->
            let editor =
              List.fold_right apply_graph_action edit_actions editor
            in
            let editors =
              Uuid.Map.remove editor.id model.editors
              |> Uuid.Map.add editor.id editor
            in
            let model = Model.{ model with editors } in
            let actions = record_actions model editor_id edit_actions in
            { model with actions })
          model editors
      in
      Model.remove_known_actions model *)

(* let apply_env (model : Model.t) (env_action : env) : Model.t option =
  match env_action with
  | Record -> (
      match model.actions with
      | None ->
          Printf.printf "Recording...\n";
          Some { model with actions = Some [] }
      | Some _ ->
          Printf.printf "(already recording)\n";
          None)
  | Report ->
      let%bind.Util.Option actions = model.actions in
      report_actions actions;
      None
  | Stop -> (
      match model.actions with
      | None ->
          Printf.printf "(already stopped)\n";
          None
      | Some actions ->
          report_actions actions;
          Printf.printf "Stopped!\n";
          Some { model with actions = None })
  | Replay str ->
      Sexplib.Sexp.of_string str |> Model.graph_action_sequence_of_sexp
      |> replay_actions model
  | Dump ->
      Model.sexp_of_t model |> Util.Sexp.print;
      None
  | Load str -> Some (Model.t_of_sexp (Sexplib.Sexp.of_string str))
  | Clone editor_id ->
      let%map.Util.Option editor = Uuid.Map.find_opt editor_id model.editors in
      let id = Uuid.Id.next () in
      let editor : Editor.t = { editor with id } in
      let editors = Uuid.Map.add id editor model.editors in
      { model with editors }
  | Drop editor_id ->
      let editors = Uuid.Map.remove editor_id model.editors in
      Some { model with editors }
  | ToggleIds editor_id ->
      let editors =
        model.editors
        |> Uuid.Map.update editor_id
             (Option.map (fun editor ->
                  Editor.{ editor with show_ids = not editor.show_ids }))
      in
      Some { model with editors } *)
