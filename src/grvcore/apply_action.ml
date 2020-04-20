let apply_action (model : Model.t) (action : Action.t) (_state : State.t)
    ~schedule_action:(_ : Action.t -> unit) : Model.t =
  match action with Increment -> { counter = model.counter + 1 }
