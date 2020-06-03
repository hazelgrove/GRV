type t = Editor.t Uuid.Map.t

let empty : t =
  let editor1 = Editor.mk () in
  let editor2 = Editor.mk () in
  (* TODO: helper for this *)
  Uuid.Map.of_seq (List.to_seq [ (editor1.id, editor1); (editor2.id, editor2) ])

let cutoff (m1 : t) (m2 : t) : bool = m1 == m2
