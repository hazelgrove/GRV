(* TODO: factor App and other ast constructor insertions into a constructor *)

(* TODO: add button to insert Var so we can test conflicts *)
type direction = In | Out | Left | Right [@@deriving sexp_of]

type inst = Create | Move of direction [@@deriving sexp_of]

type app = Send | Enqueue of inst [@@deriving sexp_of]

(* TODO: rename instance to instance_id *)

open Sexplib0.Sexp_conv

type t = { instance : int; action : app } [@@deriving sexp_of]

let apply_instance (model : Model.Instance.t) (action : inst) (_state : State.t)
    ~schedule_action:(_ : t -> unit) : Model.Instance.t =
  match action with
  | Create -> (
      let constructor = Constructor.Exp_app in
      match Index.wrap constructor with
      | None -> model
      | Some new_index ->
          let new_vertex = Vertex.mk constructor in
          let old_parent = Graph.find_vertex model.cursor.parent model.graph in
          let old_children = Graph.find_children model.cursor model.graph in
          let edge = Edge.mk old_parent model.cursor.index new_vertex in
          let graph_actions =
            [ { Graph_action.state = Edge_state.Created; edge } ]
            @ List.map
                (fun (old_edge : Edge.t) ->
                  let edge =
                    Edge.mk new_vertex new_index (Edge.target old_edge)
                  in
                  { Graph_action.state = Edge_state.Created; edge })
                (Edge.Set.elements old_children)
            @ List.map
                (fun edge ->
                  { Graph_action.state = Edge_state.Destroyed; edge })
                (Edge.Set.elements old_children)
          in
          let graph =
            List.fold_right Graph_action.apply graph_actions model.graph
          in
          { model with graph; actions = model.actions @ graph_actions } )
  | Move In ->
      let cursor =
        match
          Edge.Set.elements (Graph.find_children model.cursor model.graph)
        with
        | [ edge ] -> (
            let parent = Edge.target edge in
            match Index.down parent.value with
            | None -> model.cursor
            | Some index -> { parent; index }
            (* TODO: how to choose between ambiguous children *) )
        | _ -> model.cursor
      in
      { model with cursor }
  | Move Out ->
      let cursor =
        match
          Edge.Set.elements (Graph.find_parents model.cursor.parent model.graph)
        with
        | [ edge ] ->
            { Graph.Child.parent = Edge.source edge; index = Edge.index edge }
        | _ -> model.cursor
      in
      { model with cursor }
  | Move Left ->
      let cursor =
        match Index.left model.cursor.index with
        | None -> model.cursor
        | Some index -> { model.cursor with index }
      in
      { model with cursor }
  | Move Right ->
      let cursor =
        match Index.right model.cursor.index with
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
