module Dom_html = Js_of_ocaml.Dom_html
module Vdom = Virtual_dom.Vdom

(* TODO: where do these functions go? *)
let send (editor : Editor.t) : Action.t' Option.t =
  match Js.get_selection ("actions" ^ Uuid.Id.show editor.id) with
  | [] -> None
  | selection ->
      let actions =
        List.(
          map fst
            (filter snd
               (combine (Graph_action.Set.elements editor.actions) selection)))
      in
      Some (Comm (Send actions))

let restore (editor : Editor.t) : Action.t' Option.t =
  let selection : bool list =
    Js.get_selection ("deleted" ^ Uuid.Id.show editor.id)
  in
  let vertexes : Vertex.t list = Vertex.Set.elements (Editor.deleted editor) in
  let%map.Util.Option result =
    List.find_opt fst (List.combine selection vertexes)
  in
  let _vertex : Vertex.t = snd result in
  failwith __LOC__

let ctrl (_model : Model.t) (editor : Editor.t)
    (event : Dom_html.keyboardEvent Js.t) : Action.t' Option.t =
  match Dom_html.Keyboard_code.of_event event with
  | KeyS -> (
      match send editor with
      | None ->
          Js_of_ocaml.Dom.preventDefault event;
          Js_of_ocaml.Dom_html.stopPropagation event;
          None
      | result -> result )
  | key -> (
      (* let%map.Util.Option action : Action.t' Option.t = *)
      (* let refocus (next_id : Uuid.Id.t) (default_id : Uuid.Id.t) : unit =
           Js.focus_editor
             ( match Uuid.Map.find_opt next_id model with
             | Some _ -> next_id
             | None -> default_id )
         in *)
      match key with
      (* TODO: Map.find_least | ArrowLeft ->
             refocus (editor.id - 1) (snd @@ Model.max_binding model).id;
             None
         | ArrowRight ->
             refocus (editor.id + 1) (snd @@ Model.min_binding model).id;
             None *)
      | _ -> None )

(* in
 * Action.Enqueue action *)

let shift (event : Dom_html.keyboardEvent Js.t) : Action.t' Option.t =
  let%bind.Util.Option jstr : Js.js_string Js.t Option.t =
    Js.Optdef.to_option event##.key
  in
  (* let%map.Util.Option action : Action.local Option.t = *)
  match Js.to_string jstr with
  | "N" -> Some (Action.Edit (Create Typ_num))
  | "+" -> Some (Action.Edit (Create Exp_plus))
  | ">" -> Some (Action.Edit (Create Typ_arrow))
  | _ -> None

(* in
 * Action.Enqueue action *)

let base (editor : Editor.t) (event : Dom_html.keyboardEvent Js.t) :
    Action.t' Option.t =
  (* let%map.Util.Option action : Action.local Option.t = *)
  match Dom_html.Keyboard_code.of_event event with
  | KeyN -> (
      Js_of_ocaml.Dom.preventDefault event;
      Js_of_ocaml.Dom_html.stopPropagation event;
      match Js.focus_input "num_id" with
      | "" -> None
      | str -> Some (Action.Edit (Create (Exp_num (int_of_string str)))) )
  | KeyP -> (
      match Js.focus_input "pat_id" with
      | "" ->
          Js_of_ocaml.Dom.preventDefault event;
          Js_of_ocaml.Dom_html.stopPropagation event;
          None
      | str -> Some (Edit (Create (Pat_var str))) )
  | KeyV -> (
      match Js.focus_input "var_id" with
      | "" ->
          Js_of_ocaml.Dom.preventDefault event;
          Js_of_ocaml.Dom_html.stopPropagation event;
          None
      | str -> Some (Edit (Create (Exp_var str))) )
  | Space -> Some (Edit (Create Exp_app))
  | Backslash -> Some (Edit (Create Exp_lam))
  | Delete ->
      Js.clear_selection ("deleted" ^ Uuid.Id.show editor.id);
      Some (Edit Destroy)
  | ArrowUp -> Some (Move Up)
  | ArrowDown -> Some (Move Down)
  | ArrowLeft -> Some (Move Left)
  | ArrowRight -> Some (Move Right)
  | _ -> None

(* in
 * Action.Enqueue action *)

let dispatch ~(inject : Action.t -> Vdom.Event.t) (model : Model.t)
    (editor : Editor.t) : Dom_html.keyboardEvent Js.t -> Vdom.Event.t =
 fun event ->
  let handle =
    match
      Js.
        ( to_bool event##.shiftKey,
          to_bool event##.ctrlKey,
          to_bool event##.altKey )
    with
    | false, false, false -> base editor
    | true, false, false -> shift
    | false, true, false -> ctrl model editor
    | _, _, _ -> fun _ -> (None : Action.t' Option.t)
  in
  match handle event with
  | Some action ->
      Js_of_ocaml.Dom.preventDefault event;
      Js_of_ocaml.Dom_html.stopPropagation event;
      inject { editor_id = editor.id; action }
  | None -> Vdom.Event.Ignore
