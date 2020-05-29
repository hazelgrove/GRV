type direction = In | Out | Left | Right [@@deriving sexp_of]

type edit = Create of Lang.Constructor.t | Destroy [@@deriving sexp_of]

type local = Move of direction | Edit of edit [@@deriving sexp_of]

open Sexplib0.Sexp_conv

(* TODO: Make `Send` be to a specific instance *)
type app = Select of Cursor.t | Send of Graph_action.t list | Enqueue of local
[@@deriving sexp_of]

type t = { instance_id : int; action : app } [@@deriving sexp_of]

let apply_edit (edit : edit) (cursor : Cursor.t) (cache : Cache.t) :
    bool * Graph_action.t list =
  let old_children = Cache.children cursor cache in
  match edit with
  | Create constructor -> (
      let new_vertex = Vertex.mk constructor in
      let create_parent_edge =
        [ Graph_action.{ state = Created; edge = Edge.mk cursor new_vertex } ]
      in
      match Lang.Index.default_index constructor with
      | None -> (false, create_parent_edge)
      | Some new_index ->
          let create_new_children_edges =
            List.map
              (fun (old_edge : Edge.t) ->
                ( let source = Cursor.mk new_vertex new_index in
                  let edge = Edge.mk source old_edge.target in
                  Graph_action.{ state = Created; edge }
                  : Graph_action.t ))
              (Edge.Set.elements old_children)
          in
          let destroy_old_children_edges =
            List.map
              (fun edge -> Graph_action.{ state = Destroyed; edge })
              (Edge.Set.elements old_children)
          in
          ( true,
            create_parent_edge @ create_new_children_edges
            @ destroy_old_children_edges ) )
  | Destroy ->
      let destroy_edges =
        List.map
          (fun edge -> Graph_action.{ state = Destroyed; edge })
          (Edge.Set.elements old_children)
      in
      (false, destroy_edges)

let apply_move (d : direction) (cursor : Cursor.t) (cache : Cache.t) :
    Cursor.t option =
  match d with
  | In -> (
      match Edge.Set.elements (Cache.children cursor cache) with
      | [ edge ] -> (
          let vertex = edge.target in
          match Lang.Index.down vertex.value with
          | None -> None
          | Some index -> Some { vertex; index }
          (* TODO: how to choose between ambiguous children *) )
      | _ -> None )
  | Out -> (
      match Edge.Set.elements (Cache.parents cursor.vertex cache) with
      | [ edge ] -> Some edge.source
      | _ -> None )
  | Left -> (
      match Lang.Index.left cursor.index with
      | None -> None
      | Some index -> Some { cursor with index } )
  | Right -> (
      match Lang.Index.right cursor.index with
      | None -> None
      | Some index -> Some { cursor with index } )

let rec apply_instance (local_action : local) (model : Model.Instance.t) :
    Model.Instance.t =
  match local_action with
  | Move direction ->
      let cursor =
        Option.value ~default:model.cursor
          (apply_move direction model.cursor model.graph.cache)
      in
      { model with cursor }
  | Edit edit ->
      let allowed =
        match edit with
        | Destroy -> true
        | Create constructor ->
            Lang.Index.child_sort model.cursor.index
            = Lang.Constructor.sort_of constructor
      in
      if not allowed then model
      else
        let move_in, graph_actions =
          apply_edit edit model.cursor model.graph.cache
        in
        let graph =
          List.fold_right Graph_action.apply graph_actions model.graph
        in
        let model =
          { model with graph; actions = model.actions @ graph_actions }
        in
        if move_in then apply_instance (Move In) model else model

let apply (model : Model.t) (action : t) (_state : State.t)
    ~schedule_action:(_ : t -> unit) : Model.t =
  match action.action with
  | Select cursor -> (
      match
        let%map.Option receiver : Model.Instance.t Option.t =
          Model.MapInt.find_opt action.instance_id model
        in
        { receiver with Model.Instance.cursor }
      with
      | Some receiver -> Model.MapInt.add action.instance_id receiver model
      | None -> model )
  | Send actions ->
      let new_model =
        Model.MapInt.map
          (fun (receiver : Model.Instance.t) ->
            let graph =
              List.fold_right Graph_action.apply actions receiver.graph
            in
            { receiver with graph })
          model
      in
      Model.MapInt.update action.instance_id
        ( Option.map @@ fun (sender : Model.Instance.t) ->
          { sender with actions = [] } )
        new_model
  | Enqueue inst_action ->
      Model.MapInt.update action.instance_id
        (Option.map @@ apply_instance inst_action)
        model
