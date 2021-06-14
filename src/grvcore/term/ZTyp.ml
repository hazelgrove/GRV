type t =
  | Cursor of Typ.t
  | ZArrowT1 of Ingraph.t * t * Typ.t
  | ZArrowT2 of Ingraph.t * Typ.t * t
  | ZConflict of t * Typ.C.t

let rec erase_cursor : t -> Typ.t = function
  | Cursor ty -> ty
  | ZArrowT1 (ingraph, zty1, ty2) -> Arrow (ingraph, erase_cursor zty1, ty2)
  | ZArrowT2 (ingraph, ty1, zty2) -> Arrow (ingraph, ty1, erase_cursor zty2)
  | ZConflict (zty, conflict) ->
      Typ.Conflict (Typ.C.add (erase_cursor zty) conflict)
