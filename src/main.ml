let () =
  Incr_dom.Start_app.start ~bind_to_element_with_id:"container"
    ~initial_model:(Model.init ())
    (module App)
