type t = Cursor of Pat.t | ZConflict of t * Pat.C.t

let rec erase_cursor : t -> Pat.t = function
  | Cursor p -> p
  | ZConflict (zp, conflict) ->
      Pat.Conflict (Pat.C.add (erase_cursor zp) conflict)

let rec apply_action (action : UserAction.t) (zpat : t) (u_gen : Id.Gen.t) :
    (GraphAction.t list * Id.Gen.t) option =
  match zpat with
  | Cursor pat -> Pat.apply_action action pat u_gen
  | ZConflict (zpat, _) -> apply_action action zpat u_gen
