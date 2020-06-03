type direction = In | Out | Left | Right [@@deriving sexp_of]

type edit = Create of Lang.Constructor.t | Destroy [@@deriving sexp_of]

type local = Move of direction | Edit of edit [@@deriving sexp_of]

open Sexplib0.Sexp_conv

(* TODO: Make `Send` be to a specific editor *)
type app =
  (* TODO: Move to local *)
  | Select of Cursor.t
  | Send of Graph_action.t list
  | Enqueue of local
[@@deriving sexp_of]

type t = { editor_id : Uuid.Id.t; action : app } [@@deriving sexp_of]

module Global = struct
  let filter_editor_actions (globally_known : Graph_action.Set.t)
      (editor : Editor.t) : Editor.t =
    let actions =
      List.filter
        (fun a -> not (Graph_action.Set.mem a globally_known))
        editor.value.actions
    in
    { editor with value = { editor.value with actions } }

  let globally_known_actions (_model : Model.t) : Graph_action.Set.t =
    Graph_action.Set.empty

  (* let knowns =
       List.map
         (fun (_, i) -> i.graph.cache.known_actions)
         (Uuid.Map.bindings model.instances)
     in
     List.fold_left (fun x z -> Graph_action.Set.union .... ) knowns *)

  let remove_known_actions (model : Model.t) : Model.t =
    let known_actions : Graph_action.Set.t = globally_known_actions model in
    let f : Editor.t -> Editor.t = filter_editor_actions known_actions in
    Uuid.Map.map f model
end

let apply_edit (edit : edit) (cursor : Cursor.t) (cache : Cache.t) :
    bool * Graph_action.t list =
  let old_children = Cache.children cursor cache in
  match edit with
  | Create constructor -> (
      let new_vertex = Vertex.mk constructor in
      let create_parent_edge =
        [
          Uuid.Wrap.mk
            Graph_action.{ state = Created; edge = Edge.mk cursor new_vertex };
        ]
      in
      match Lang.Index.default_index constructor with
      | None -> (false, create_parent_edge)
      | Some new_index ->
          let create_new_children_edges =
            List.map
              (fun (old_edge : Edge.t) ->
                ( let source = Cursor.mk new_vertex new_index in
                  let edge = Edge.mk source (Edge.target old_edge) in
                  Uuid.Wrap.mk Graph_action.{ state = Created; edge }
                  : Graph_action.t ))
              (Edge.Set.elements old_children)
          in
          let destroy_old_children_edges =
            List.map
              (fun edge ->
                Uuid.Wrap.mk Graph_action.{ state = Destroyed; edge })
              (Edge.Set.elements old_children)
          in
          ( true,
            create_parent_edge @ create_new_children_edges
            @ destroy_old_children_edges ) )
  | Destroy ->
      let destroy_edges =
        List.map
          (fun edge -> Uuid.Wrap.mk Graph_action.{ state = Destroyed; edge })
          (Edge.Set.elements old_children)
      in
      (false, destroy_edges)

let apply_move (d : direction) (cursor : Cursor.t) (cache : Cache.t) :
    Cursor.t option =
  match d with
  | In -> (
      match Edge.Set.elements (Cache.children cursor cache) with
      | [ edge ] -> (
          let vertex = Edge.target edge in
          match Lang.Index.down vertex.value with
          | None -> None
          | Some index -> Some { vertex; index }
          (* TODO: how to choose between ambiguous children *) )
      | _ -> None )
  | Out -> (
      match Edge.Set.elements (Cache.parents cursor.vertex cache) with
      | [ edge ] -> Some (Edge.source edge)
      | _ -> None )
  | Left -> (
      match Lang.Index.left cursor.index with
      | None -> None
      | Some index -> Some { cursor with index } )
  | Right -> (
      match Lang.Index.right cursor.index with
      | None -> None
      | Some index -> Some { cursor with index } )

let rec apply_editor (local_action : local) (editor : Editor.t) : Editor.t =
  match local_action with
  | Move direction ->
      let cursor =
        Option.value ~default:editor.value.cursor
          (apply_move direction editor.value.cursor editor.value.graph.cache)
      in
      { editor with value = { editor.value with cursor } }
  | Edit edit ->
      let allowed =
        match edit with
        | Destroy -> true
        | Create constructor ->
            Lang.Index.child_sort editor.value.cursor.index
            = Lang.Constructor.sort_of constructor
      in
      if not allowed then editor
      else
        let move_in, graph_actions =
          apply_edit edit editor.value.cursor editor.value.graph.cache
        in
        let graph =
          List.fold_right Graph_action.apply graph_actions editor.value.graph
        in
        let value =
          {
            editor.value with
            graph;
            actions = editor.value.actions @ graph_actions;
          }
        in
        let editor = { editor with value } in
        if move_in then apply_editor (Move In) editor else editor

let apply (model : Model.t) (action : t) (_state : State.t)
    ~schedule_action:(_ : t -> unit) : Model.t =
  match action.action with
  | Select cursor -> (
      match
        let%map.Util.Option editor : Editor.t Option.t =
          Uuid.Map.find_opt action.editor_id model
        in
        (* TODO: fix this *)
        let editor : Editor.t = editor in
        { editor with value = { editor.value with cursor } }
      with
      | Some editor -> Uuid.Map.add action.editor_id editor model
      | None -> model )
  | Send actions ->
      let new_model : Model.t =
        Uuid.Map.map
          (fun (editor : Editor.t) ->
            let graph =
              List.fold_right Graph_action.apply actions editor.value.graph
            in
            { editor with value = { editor.value with graph } })
          model
      in
      Global.remove_known_actions new_model
  | Enqueue edit_action ->
      Uuid.Map.update action.editor_id
        (Option.map @@ apply_editor edit_action)
        model
