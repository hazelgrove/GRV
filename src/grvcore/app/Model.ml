type t = Editor.t Uuid.Map.t

let empty : t =
  let editor1 = Editor.mk () in
  let editor2 = Editor.mk () in
  (* TODO: helper for this *)
  Uuid.Map.of_seq (List.to_seq [ (editor1.id, editor1); (editor2.id, editor2) ])

let cutoff (m1 : t) (m2 : t) : bool = m1 == m2

let filter_editor_actions (globally_known : Graph_action.Set.t)
    (editor : Editor.t) : Editor.t =
  let actions =
    Graph_action.Set.filter
      (fun a -> not (Graph_action.Set.mem a globally_known))
      editor.actions
  in
  { editor with actions }

let globally_known_actions (model : t) : Graph_action.Set.t =
  let knowns =
    List.map
      (fun (_, (e : Editor.t)) -> e.known_actions)
      (Uuid.Map.bindings model)
  in
  List.fold_left Graph_action.Set.inter (List.hd knowns) knowns

let remove_known_actions (model : t) : t =
  let known_actions = globally_known_actions model in
  Uuid.Map.map (filter_editor_actions known_actions) model
