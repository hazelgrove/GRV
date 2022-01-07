open Bonsai_web;

let _: Start.Handle.t(_) =
  Start.start_standalone(
    ~initial_input=(),
    ~bind_to_element_with_id="container",
    Bonsai.of_module((module App), ~default_model=()),
  );
