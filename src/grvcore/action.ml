(* TODO: factor App and other ast constructor insertions into a constructor *)

type t = Create | Move of direction [@@deriving sexp_of]

and direction = In | Out | Left | Right [@@deriving sexp_of]

let apply (model : Model.t) (action : t) (_state : State.t)
    ~schedule_action:(_ : t -> unit) : Model.t =
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
            [ { Graph_action.state = Edge.Created; edge } ]
            @ List.map
                (fun (old_edge : Edge.t) ->
                  let edge =
                    Edge.mk new_vertex new_index (Edge.target old_edge)
                  in
                  { Graph_action.state = Edge.Created; edge })
                (Edge.Set.elements old_children)
            @ List.map
                (fun edge -> { Graph_action.state = Edge.Destroyed; edge })
                (Edge.Set.elements old_children)
          in
          let graph =
            List.fold_right Graph_action.apply graph_actions model.graph
          in
          { model with graph } )
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
