include Js_of_ocaml.Js

let claim_event (event : 'a t) : unit =
  Js_of_ocaml.Dom.preventDefault event;
  Js_of_ocaml.Dom_html.stopPropagation event

let eval_to_string (js : string) : string = to_string (Unsafe.eval_string js)

let eval_to_unit (js : string) : unit =
  let _ = Unsafe.eval_string js in
  ()

let focus (id : string) : unit =
  eval_to_unit ("setTimeout(function(){refocus('" ^ id ^ "')}, 0)")

let get_input (id : string) : string =
  eval_to_string @@ "getInput('" ^ id ^ "')"

let set_input (id : string) (value : string) : unit =
  eval_to_unit @@ Printf.sprintf "setInput('%s', '%s')" id value

let focus_input (id : string) : string =
  focus id;
  get_input id

let draw_viz (id : Uuid.Id.t) (dot_src : string) : unit =
  eval_to_unit
  @@ Printf.sprintf "drawViz('graph%s', '%s')" (Uuid.Id.to_string id) dot_src

let get_selection (id : string) : bool list =
  Array.to_list @@ to_array @@ Unsafe.eval_string ("getSelection('" ^ id ^ "')")

let clear_selection (id : string) : unit =
  eval_to_unit @@ "clearSelection('" ^ id ^ "')"

let fill_selection (id : string) : unit =
  eval_to_unit @@ "fillSelection('" ^ id ^ "')"

let toggle_item (id : string) (i : int) : unit =
  eval_to_unit @@ "toggleItem('" ^ id ^ "', " ^ Int.to_string i ^ ")"

let select_item (id : string) (i : int) : unit =
  eval_to_unit @@ "selectItem('" ^ id ^ "', " ^ Int.to_string i ^ ")"

let prompt (message : string) : string =
  eval_to_string @@ "window.prompt('" ^ message ^ "') || ''"
