(* Entrypoint to application, creates the environment with 2 editors *)
let () =
  Incr_dom.Start_app.start
    (module App)
    ~bind_to_element_with_id:"container" ~initial_model:(Model.mk ())
