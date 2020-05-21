include Js_of_ocaml.Js

let eval_to_string (js : string) : string = to_string @@ Unsafe.eval_string js

let eval_to_unit (js : string) : unit =
  let _ = Unsafe.eval_string js in
  ()
