type t = { editors : Editor.t Uuid.Map.t; known_actions : Graph_action.Set.t }

let empty : t =
  let editor1 = Editor.mk () in
  let editor2 = Editor.mk () in
  (* TODO: helper for this *)
  let editors =
    Uuid.Map.of_seq
      (List.to_seq [ (editor1.id, editor1); (editor2.id, editor2) ])
  in
  { editors; known_actions = Graph_action.Set.empty }

let cutoff (m1 : t) (m2 : t) : bool = m1 == m2

let filter_editor_actions (globally_known : Graph_action.Set.t)
    (editor : Editor.t) : Editor.t =
  let actions =
    List.filter
      (fun a -> not (Graph_action.Set.mem a globally_known))
      editor.value.actions
  in
  { editor with value = { editor.value with actions } }

let globally_known_actions (model : t) : Graph_action.Set.t =
  model.known_actions

(* let knowns =
     List.map
       (fun (_, i) -> i.graph.cache.known_actions)
       (Uuid.Map.bindings model.instances)
   in
   List.fold_left (fun x z -> Graph_action.Set.union .... ) knowns *)

let remove_known_actions (model : t) : t =
  let editors =
    Uuid.Map.map (filter_editor_actions model.known_actions) model.editors
  in
  { model with editors }
