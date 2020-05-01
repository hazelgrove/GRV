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
      let graph =
        let target = Vertex.(mk Exp_app) in
        let source = Graph.find_vertex model.cursor_ref.parent model.graph in
        let edge = Edge.mk source model.cursor_ref.index target in
        Graph.update_edge model.graph edge Edge.Created
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
