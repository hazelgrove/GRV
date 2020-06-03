include Js_of_ocaml.Js

let eval_to_string (js : string) : string = to_string (Unsafe.eval_string js)

let eval_to_unit (js : string) : unit =
  let _ = Unsafe.eval_string js in
  ()

let focus (id : string) : unit = eval_to_unit ("refocus('" ^ id ^ "')")

let focus_editor (id : Uuid.Id.t) : unit = focus ("editor" ^ Uuid.Id.show id)

let get_input (id : string) : string =
  eval_to_string @@ "getInput('" ^ id ^ "')"

let set_input (id : string) (value : string) : unit =
  eval_to_unit @@ Printf.sprintf "setInput('%s', '%s')" id value

let focus_input (id : string) : string =
  focus id;
  get_input id

let draw_viz (id : Uuid.Id.t) (dot_src : string) : unit =
  eval_to_unit
  @@ Printf.sprintf "drawViz('graph%s', '%s')" (Uuid.Id.show id) dot_src

let get_selection (id : string) : bool list =
  Array.to_list @@ to_array @@ Unsafe.eval_string ("getSelection('" ^ id ^ "')")

let clear_selection (id : string) : unit =
  eval_to_unit ("clearSelection('" ^ id ^ "')")

let fill_selection (id : string) : unit =
  eval_to_unit ("fillSelection('" ^ id ^ "')")
