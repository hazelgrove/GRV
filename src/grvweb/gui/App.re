// open Bonsai_web;
open Virtual_dom.Vdom;
// open Sexplib.Std;

module Input = {
  type t = unit;
};

module Model = {
  type t = unit;
  // type t = {editors: list(component(Editor.t))};

  let equal = (==);
};

module Action = {
  type t = unit;
};

module Result = Node;

let apply_action =
    (
      ~inject as _: Action.t => Event.t,
      ~schedule_event as _: Event.t => unit,
      (): Input.t,
      (): Model.t,
    )
    : (Action.t => Model.t) =>
  fun
  | () => ();

// let compute =
//     (~inject as _: Action.t => Event.t, (): Input.t, (): Model.t): Result.t => {
//   // let default_model = Id.Ctx.(Editor.Model.init(Id.Gen.init) |> fst);
//   // let editor = Bonsai.of_module((module Editor), ~default_model);
// };

// module Action = {
//   type t =
//     | Increment
//     | Decrement;
// };

// let apply_action =
//     (
//       ~inject as _: Action.t => Event.t,
//       ~schedule_event as _: Event.t => unit,
//       (): Input.t,
//       model: Model.t,
//     )
//     : (Action.t => Model.t) =>
//   fun
//   | Increment => model + 1
//   | Decrement => model - 1;

// let compute =
//     (~inject: Action.t => Event.t, (): Input.t, model: Model.t): Result.t => {
//   let button = (label, action) =>
//     Node.button([Attr.on_click(_ => inject(action))], [Node.text(label)]);
//   Node.div(
//     [],
//     [
//       button("-1", Action.Decrement),
//       Node.text(Int.to_string(model)),
//       button("+1", Action.Increment),
//     ],
//   );
// };

let name = "App";
