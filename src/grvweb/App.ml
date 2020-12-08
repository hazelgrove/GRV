open Incr_dom
module Model = Model
module Action = Action
module State = State

let on_startup ~schedule_action:(_ : Action.t -> unit) (_ : Model.t) :
    State.t Async_kernel.Deferred.t =
  Async_kernel.Deferred.return State.State

let create (model : Model.t Incr.t) ~old_model:(_ : Model.t Incr.t)
    ~(inject : Action.t -> Vdom.Event.t) :
    (Action.t, Model.t, State.t) Component.t Incr.t =
  let%map.Incr model = model in
  let view = View.view ~inject model in
  Component.create model view ~apply_action:(Action.apply model)
