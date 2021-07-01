open OptionUtil.Syntax

type t = Cursor of Pat.t | ZConflict of t * Pat.C.t [@@deriving sexp]

let rec erase_cursor : t -> Pat.t = function
  | Cursor p -> p
  | ZConflict (zp, conflict) ->
      Pat.Conflict (Pat.C.add (erase_cursor zp) conflict)

let rec move (move_action : UserAction.move) (zpat : t) : t option =
  let rec up : t -> t option = function
    | Cursor _ -> None
    | ZConflict (Cursor pat, conflict) ->
        let conflict = Pat.C.add pat conflict in
        Some (Cursor (Conflict conflict))
    | ZConflict (zpat', conflict) ->
        let+ zpat' = up zpat' in
        ZConflict (zpat', conflict)
  in

  let rec down : t -> t option = function
    | Cursor pat -> (
        match pat with
        | Var (_, _) | Multiparent _ | Unicycle _ | Hole (_, _) -> None
        | Conflict conflict ->
            let+ pat' = Pat.C.min_elt_opt conflict in
            let conflict = Pat.C.remove pat' conflict in
            ZConflict (Cursor pat', conflict))
    | ZConflict (zpat, conflict) ->
        let+ zpat = down zpat in
        ZConflict (zpat, conflict)
  in

  let rec left : t -> t option = function
    | Cursor _ -> None
    | ZConflict (Cursor pat, conflict) ->
        let less, present, greater = Pat.C.split pat conflict in
        if not present then None
        else
          let conflict' = Pat.C.add pat conflict in
          let+ pat' = Pat.C.max_elt_opt less in
          ZConflict (Cursor pat', conflict')
    | ZConflict (zpat, conflict) ->
        let+ zpat = left zpat in
        ZConflict (zpat, conflict)
  in

  let rec right : t -> t option = function
    | Cursor _ -> None
    | ZConflict (Cursor pat, conflict) ->
        let less, present, greater = Pat.C.split pat conflict in
        if not present then None
        else
          let conflict' = Pat.C.add pat conflict in
          let+ pat' = Pat.C.min_elt_opt greater in
          ZConflict (Cursor pat', conflict')
    | ZConflict (zpat, conflict) ->
        let+ zpat = right zpat in
        ZConflict (zpat, conflict)
  in

  let select () : t option = ( ?? ) in

  match move_action with
  | Up -> up zpat
  | Down -> down zpat
  | Left -> left zpat
  | Right -> right zpat
  | Select _ -> select ()

let rec edit (edit_action : UserAction.edit) (zpat : t) (u_gen : Id.Gen.t) :
    (GraphAction.t list * Id.Gen.t) option =
  match zpat with
  | Cursor pat -> Pat.edit edit_action pat u_gen
  | ZConflict (zpat, _) -> edit edit_action zpat u_gen
