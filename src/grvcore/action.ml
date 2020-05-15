(* TODO: factor App and other ast constructor insertions into a constructor *)

(* TODO: add button to insert Var so we can test conflicts *)
type direction = In | Out | Left | Right [@@deriving sexp_of]

(* TODO: factor Create and Delete into own type *)
type inst = Create | Delete | Move of direction [@@deriving sexp_of]

type app = Send | Enqueue of inst [@@deriving sexp_of]

(* TODO: rename instance to instance_id *)

open Sexplib0.Sexp_conv

type t = { instance : int; action : app } [@@deriving sexp_of]

let rec apply_instance (model : Model.Instance.t) (action : inst)
    (state : State.t) ~(schedule_action : t -> unit) : Model.Instance.t =
  let graph = model.graph in
  match action with
  | Create -> (
      let constructor = Lang.Constructor.Exp_app in
      match Lang.Index.wrap constructor with
      | None -> model
      | Some new_index ->
          let new_vertex = Vertex.mk constructor in
          let old_parent = Cache.vertex model.cursor.vertex graph.cache in
          let old_children = Cache.children model.cursor graph.cache in
          let cursor = Cursor.mk old_parent model.cursor.index in
          (* TODO: replace with model.cursor *)
          let edge = Edge.mk cursor new_vertex in
          let graph_actions =
            [ { Graph_action.state = Edge_state.Created; edge } ]
            @ List.map
                (fun (old_edge : Edge.t) ->
                  let source = Cursor.mk new_vertex new_index in
                  let edge = Edge.mk source (Edge.target old_edge) in
                  { Graph_action.state = Edge_state.Created; edge })
                (Edge.Set.elements old_children)
            @ List.map
                (fun edge ->
                  { Graph_action.state = Edge_state.Destroyed; edge })
                (Edge.Set.elements old_children)
          in
          let graph = List.fold_right Graph_action.apply graph_actions graph in
          (* always Move In after Create *)
          let model =
            { model with graph; actions = model.actions @ graph_actions }
          in
          apply_instance model (Move In) state ~schedule_action )
  | Delete ->
      let old_children = Cache.children model.cursor graph.cache in
      let graph_actions =
        List.map
          (fun edge -> { Graph_action.state = Edge_state.Destroyed; edge })
          (Edge.Set.elements old_children)
      in
      let graph = List.fold_right Graph_action.apply graph_actions graph in
      { model with graph; actions = model.actions @ graph_actions }
  | Move In ->
      let cursor =
        match Edge.Set.elements (Cache.children model.cursor graph.cache) with
        | [ edge ] -> (
            let vertex = Edge.target edge in
            match Lang.Index.down vertex.value with
            | None -> model.cursor
            | Some index -> { vertex; index }
            (* TODO: how to choose between ambiguous children *) )
        | _ -> model.cursor
      in
      { model with cursor }
  | Move Out ->
      let cursor =
        match
          Edge.Set.elements (Cache.parents model.cursor.vertex graph.cache)
        with
        | [ edge ] -> Edge.source edge
        | _ -> model.cursor
      in
      { model with cursor }
  | Move Left ->
      let cursor =
        match Lang.Index.left model.cursor.index with
        | None -> model.cursor
        | Some index -> { model.cursor with index }
      in
      { model with cursor }
  | Move Right ->
      let cursor =
        match Lang.Index.right model.cursor.index with
        | None -> model.cursor
        | Some index -> { model.cursor with index }
      in
      { model with cursor }

let apply (model : Model.t) (action : t) (state : State.t)
    ~(schedule_action : t -> unit) : Model.t =
  match action.action with
  | Send ->
      let actions = (Model.MapInt.find action.instance model).actions in
      let new_model =
        Model.MapInt.map
          (fun (receiver : Model.Instance.t) ->
            let graph =
              List.fold_right Graph_action.apply actions receiver.graph
            in
            { receiver with graph })
          model
      in
      Model.MapInt.update action.instance
        ( Option.map @@ fun (sender : Model.Instance.t) ->
          { sender with actions = [] } )
        new_model
  | Enqueue inst_action ->
      Model.MapInt.update action.instance
        ( Option.map @@ fun instance ->
          apply_instance instance inst_action state ~schedule_action )
        model
