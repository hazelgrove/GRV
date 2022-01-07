open Virtual_dom.Vdom;
open Sexplib.Std;

module Input = {
  type t = unit;
};

module Model = {
  [@deriving sexp]
  type t = unit;

  let equal = (==);
  let default: t = ();
};

module Action = {
  [@deriving sexp]
  type t;
};

module Result = Node;

let apply_action =
    (
      ~inject as _: Action.t => Event.t,
      ~schedule_event as _: Event.t => unit,
      _: Input.t,
      model: Model.t,
      _: Action.t,
    )
    : Model.t => model;

let compute =
    (~inject as _: Action.t => Event.t, _: Input.t, _: Model.t): Result.t =>
  Node.text("Hello, again!");

let name = "GRV";
