open Incr_dom

(* Sub-modules defined in other files *)
module Model = Model
module Action = Action
module State = State

let on_startup ~schedule_action:(_ : Action.t -> unit) (_ : Model.t) :
    State.t Async_kernel.Deferred.t =
  Async_kernel.Deferred.return (State.init ())

let apply_action (model : Model.t) (action : Action.t) (_state : State.t)
    ~schedule_action:(_ : Action.t -> unit) : Model.t =
  match action with Increment -> { counter = model.counter + 1 }

let create (model : Model.t Incr.t) ~old_model:(_ : Model.t Incr.t)
    ~(inject : Action.t -> Vdom.Event.t) :
    (Action.t, Model.t, State.t) Component.t Incr.t =
  let open Incr.Let_syntax in
  let%map model = model in
  let view = View.view ~inject model in
  Component.create model ~apply_action:(apply_action model)
    (* -> ?update_visibility:(schedule_action:('action -> unit) -> 'model)*)
    (*-> ?on_display:('state -> schedule_action:('action -> unit) -> unit)*)
    view
