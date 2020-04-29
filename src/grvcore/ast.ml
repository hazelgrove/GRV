module HExp = struct
  type t = t' Uuid.with_uuid

  and t' = App of t * t | EmptyHole
end

type hexp = HExp.t

let rec walk_to (exp : hexp) (cursor : Cursor.t) : hexp =
  match cursor with
  | Here -> exp
  | To (side, cursor') -> (
      match (side, Uuid.unwrap exp) with
      | `Left, App (e1, _e2) -> walk_to e1 cursor'
      | `Right, App (_e1, e2) -> walk_to e2 cursor'
      | _ ->
          Printf.printf "error: invalid cursor position";
          exp )

let rec apply_at (exp : hexp) (cursor : Cursor.t) (f : hexp -> hexp) : hexp =
  match cursor with
  | Here -> f exp
  | To (side, cursor') ->
      let recur () =
        let open HExp in
        match (side, Uuid.unwrap exp) with
        | `Left, App (e1, e2) -> App (apply_at e1 cursor' f, e2)
        | `Right, App (e1, e2) -> App (e1, apply_at e2 cursor' f)
        | _ ->
            Printf.printf "error: invalid cursor position";
            Uuid.unwrap exp
      in
      { uuid = exp.uuid; value = recur () }
