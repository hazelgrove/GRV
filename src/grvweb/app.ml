open Incr_dom
module Deferred = Async_kernel.Deferred

(* Sub-modules defined in other files *)
module Model = Model
module Action = Action
module State = State

let on_startup ~schedule_action:(_ : Action.t -> unit) (_ : Model.t) :
    State.t Deferred.t =
  Deferred.return @@ ref false

let create (model : Model.t Incr.t) ~old_model:(_ : Model.t Incr.t)
    ~(inject : Action.t -> Vdom.Event.t) :
    (Action.t, Model.t, State.t) Component.t Incr.t =
  let open Incr.Let_syntax in
  let%map model = model in
  let view = View.view ~inject model in
  Component.create model view ~apply_action:(Action.apply model)
