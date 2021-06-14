type t = Cursor of Pat.t | ZConflict of t * Pat.C.t

let rec erase_cursor : t -> Pat.t = function
  | Cursor p -> p
  | ZConflict (zp, conflict) ->
      Pat.Conflict (Pat.C.add (erase_cursor zp) conflict)
