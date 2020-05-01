module HExp = struct
  type t = t' Uuid.t

  and t' = App of t * t | EmptyHole
end

let rec walk_to (exp : HExp.t) (cursor : Cursor.t) : HExp.t =
  match cursor with
  | Here -> exp
  | To (side, cursor') ->
      Uuid.bind exp (fun (_, e) ->
          match (side, e) with
          | Left, App (e1, _e2) -> walk_to e1 cursor'
          | Right, App (_e1, e2) -> walk_to e2 cursor'
          | _ ->
              Printf.printf "error: invalid cursor position";
              exp)

let rec apply_at (exp : HExp.t) (cursor : Cursor.t) (f : HExp.t -> HExp.t) :
    HExp.t =
  match cursor with
  | Here -> f exp
  | To (side, cursor') ->
      let open HExp in
      Uuid.(
        bind exp (fun (_, e) ->
            match (side, e) with
            | Left, App (e1, e2) -> return @@ App (apply_at e1 cursor' f, e2)
            | Right, App (e1, e2) -> return @@ App (e1, apply_at e2 cursor' f)
            | _ ->
                Printf.printf "error: invalid cursor position";
                exp))
