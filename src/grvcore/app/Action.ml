type move = Left | Right | Up | Down | Select of Cursor.t [@@deriving sexp_of]

type edit = Create of Lang.Constructor.t | Destroy [@@deriving sexp_of]

open Sexplib0.Sexp_conv

(* TODO: Make `Send` be to a specific editor *)
type comm = Send of Graph_action.t list [@@deriving sexp_of]

type t' = Move of move | Edit of edit | Comm of comm [@@deriving sexp_of]

type t = { editor_id : Uuid.Id.t; action : t' } [@@deriving sexp_of]

let apply_move (model : Model.t) (editor_id : Uuid.Id.t) (move_action : move) :
    Model.t Option.t =
  let%bind.Util.Option editor = Uuid.Map.find_opt editor_id model.editors in
  let cache : Cache.t = editor.value.graph.cache in
  let cursor : Cursor.t = editor.value.cursor in
  let cursor : Cursor.t Option.t =
    match move_action with
    | Left ->
        let%map.Util.Option index = Lang.Index.left cursor.index in
        { cursor with index }
    | Right ->
        let%map.Util.Option index = Lang.Index.right cursor.index in
        { cursor with index }
    | Up -> (
        match Edge.Set.elements (Cache.parents cursor.vertex cache) with
        | [ edge ] -> Some (Edge.source edge)
        | _ -> None )
    | Down -> (
        match Edge.Set.elements (Cache.children cursor cache) with
        | [ edge ] ->
            let vertex = Edge.target edge in
            let%map.Util.Option index = Lang.Index.down vertex.value in
            { Cursor.vertex; index }
            (* TODO: how to choose between ambiguous children *)
        | _ -> None )
    | Select cursor -> Some cursor
  in
  let%map.Util.Option cursor = cursor in
  let editor = { editor with value = { editor.value with cursor } } in
  let editors = Uuid.Map.add editor_id editor model.editors in
  { model with editors }

let apply_edit (model : Model.t) (editor_id : Uuid.Id.t) (edit_action : edit) :
    Model.t Option.t =
  let%bind.Util.Option editor = Uuid.Map.find_opt editor_id model.editors in
  let children = Cache.children editor.value.cursor editor.value.graph.cache in
  let move_in, graph_actions =
    match edit_action with
    | Create constructor -> (
        let vertex : Vertex.t = Vertex.mk constructor in
        let create_parent_edge =
          [
            Uuid.Wrap.mk
              Graph_action.
                { state = Created; edge = Edge.mk editor.value.cursor vertex };
          ]
        in
        match Lang.Index.default_index constructor with
        | None -> (false, create_parent_edge)
        | Some index ->
            let create_new_children_edges =
              List.map
                (fun (old_edge : Edge.t) ->
                  ( let source = Cursor.mk vertex index in
                    let edge = Edge.mk source (Edge.target old_edge) in
                    Uuid.Wrap.mk Graph_action.{ state = Created; edge }
                    : Graph_action.t ))
                (Edge.Set.elements children)
            in
            let destroy_old_children_edges =
              List.map
                (fun edge ->
                  Uuid.Wrap.mk Graph_action.{ state = Destroyed; edge })
                (Edge.Set.elements children)
            in
            ( true,
              create_parent_edge @ create_new_children_edges
              @ destroy_old_children_edges ) )
    | Destroy ->
        ( false,
          List.map
            (fun edge -> Uuid.Wrap.mk Graph_action.{ state = Destroyed; edge })
            (Edge.Set.elements
               (Cache.children editor.value.cursor editor.value.graph.cache)) )
  in
  let graph =
    List.fold_right Graph_action.apply graph_actions editor.value.graph
  in
  let actions = editor.value.actions @ graph_actions in
  let editor = { editor with value = { editor.value with graph; actions } } in
  let editors = Uuid.Map.add editor_id editor model.editors in
  let model = { model with editors } in
  if move_in then apply_move model editor_id Down else Some model

let apply_comm (model : Model.t) (_editor_id : Uuid.Id.t) (comm_action : comm) :
    Model.t Option.t =
  match comm_action with
  | Send edit_actions ->
      let editors : Editor.t Uuid.Map.t =
        Uuid.Map.map
          (fun (editor : Editor.t) ->
            let graph =
              List.fold_right Graph_action.apply edit_actions editor.value.graph
            in
            { editor with value = { editor.value with graph } })
          model.editors
      in
      let model = { model with editors } in
      Some (Model.remove_known_actions model)

let apply (model : Model.t) (action : t) (_state : State.t)
    ~schedule_action:(_ : t -> unit) : Model.t =
  Option.value
    ( match action.action with
    | Move move_action -> apply_move model action.editor_id move_action
    | Edit edit_action -> apply_edit model action.editor_id edit_action
    | Comm comm_action -> apply_comm model action.editor_id comm_action )
    ~default:model
