let apply_action (model : Model.t) (action : Action.t) (_state : State.t)
    ~schedule_action:(_ : Action.t -> unit) : Model.t =
  match action with
  | App ->
      {
        model with
        ast = Uuid.wrap (Ast.HExp.App (model.ast, Uuid.wrap Ast.HExp.EmptyHole));
      }
  | MoveIn -> { model with cursor = model.cursor @ [ 0 ] }
