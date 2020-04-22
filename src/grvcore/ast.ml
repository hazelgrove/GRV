module HExp = struct
  type t = t' Uuid.with_uuid

  and t' = App of t * t | EmptyHole

  let rec apply_at_exp (f : t -> t) (c : Cursor.t) (exp : t) : t =
    match c with
    | [] -> f exp
    | c :: cs ->
        {
          uuid = exp.uuid;
          value =
            ( match (c, exp.value) with
            | 0, App (exp1, exp2) -> App (apply_at_exp f cs exp1, exp2)
            | 1, App (exp1, exp2) -> App (exp1, apply_at_exp f cs exp2)
            | _ -> Printf.printf "error: invalid cursor position"; exp.value );
        }
end
