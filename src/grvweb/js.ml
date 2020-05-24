include Js_of_ocaml.Js

let eval_to_string (js : string) : string = to_string (Unsafe.eval_string js)

let eval_to_unit (js : string) : unit =
  let _ = Unsafe.eval_string js in
  ()

let focus (id : string) : unit = eval_to_unit ("refocus('" ^ id ^ "')")

let focus_instance (id : int) : unit = focus ("instance" ^ Int.to_string id)

let get_input (id : string) : string =
  eval_to_string @@ "getInput('" ^ id ^ "')"

let set_input (id : string) (value : string) : unit =
  eval_to_unit @@ Printf.sprintf "setInput('%s', '%s')" id value

let focus_input (id : string) : string =
  focus id;
  get_input id

let draw_viz (id : int) (dot_src : string) : unit =
  eval_to_unit @@ Printf.sprintf "drawViz('graph%d', '%s')" id dot_src
