let print_actions (model : Model.t) : unit =
  match model.actions with
  | None -> ()
  | Some actions ->
      print_endline
        (Sexplib.Sexp.to_string (Graph_action.sexp_of_sequence actions))
