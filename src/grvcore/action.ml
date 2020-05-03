(* TODO: factor App and other ast constructor insertions into a constructor *)

type t = Create | Move of direction [@@deriving sexp_of]

and direction = In | Out | Left | Right [@@deriving sexp_of]

let apply (model : Model.t) (action : t) (_state : State.t)
    ~schedule_action:(_ : t -> unit) : Model.t =
  let open Ast in
  let open Ast.HExp in
  match action with
  | Create ->
      (* TODO:
           - Make movement actions work on cursor
           - Take existing children of the cursor:
               1. Remove edges pointing to children
               2. Add edges making them children of v
      *)
      let ast =
        apply_at model.ast model.cursor (fun exp ->
            Uuid.wrap @@ App (exp, Uuid.wrap EmptyHole))
      in
      let graph_actions =
        let new_vertex = Vertex.(mk Exp_app) in
        let old_parent =
          Graph.find_vertex model.cursor_ref.parent model.graph
        in
        let old_children = Graph.find_children model.cursor_ref model.graph in
        let edge = Edge.mk old_parent model.cursor_ref.index new_vertex in
        [ { Graph_action.state = Edge.Created; edge } ]
        @ List.map
            (fun (old_edge : Edge.t) ->
              let edge =
                Edge.mk new_vertex (Edge.index old_edge) (Edge.target old_edge)
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
      { model with ast; graph }
  | Move In ->
      let cursor =
        match Uuid.unwrap (walk_to model.ast model.cursor) with
        | App _ -> Cursor.push Left model.cursor
        | EmptyHole -> model.cursor
      in
      { model with cursor }
  | Move Out -> { model with cursor = Cursor.pop model.cursor }
  | Move Left ->
      let cursor =
        match Cursor.last model.cursor with
        | To (Right, _) -> Cursor.extend model.cursor Left
        | To (Left, _) | Here -> model.cursor
      in
      { model with cursor }
  | Move Right ->
      let cursor =
        match Cursor.last model.cursor with
        | To (Left, _) -> Cursor.extend model.cursor Right
        | To (Right, _) | Here -> model.cursor
      in
      { model with cursor }
