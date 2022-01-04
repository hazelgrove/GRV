open Bonsai_web;

module Model = {
  open Sexplib.Std;

  [@deriving sexp]
  type t = unit;

  let equal = (==);
  let default: t = ();
};

let _: Start.Handle.t(_) =
  Start.start_standalone(
    ~initial_input=(),
    ~bind_to_element_with_id="container",
    Bonsai.const(Virtual_dom.Vdom.Node.text("Hello, world!")),
  );
