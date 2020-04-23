open Cursor

let unwrap, wrap = (Uuid.unwrap, Uuid.wrap)

module HExp = struct
  type t = t' Uuid.with_uuid

  and t' = App of t * t | EmptyHole

  let rec walk_to (exp : t) (cursor : Cursor.t) : t =
    match cursor with
    | Here -> exp
    | To (side, cursor') -> (
        match (side, unwrap exp) with
        | Left, App (e1, _e2) -> walk_to e1 cursor'
        | Right, App (_e1, e2) -> walk_to e2 cursor'
        | _ ->
            Printf.printf "error: invalid cursor position";
            exp )

  let rec apply_at (exp : t) (cursor : Cursor.t) (f : t -> t) : t =
    match cursor with
    | Here -> f exp
    | To (side, cursor') ->
        let open Cursor in
        let recur () =
          match (side, unwrap exp) with
          | Left, App (e1, e2) -> App (apply_at e1 cursor' f, e2)
          | Right, App (e1, e2) -> App (e1, apply_at e2 cursor' f)
          | _ ->
              Printf.printf "error: invalid cursor position";
              unwrap exp
        in
        { uuid = exp.uuid; value = recur () }
end
