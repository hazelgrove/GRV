type t =
  | Cursor of Exp.t
  | ZLamP of Ingraph.t * ZPat.t * Typ.t * Exp.t
  | ZLamT of Ingraph.t * Pat.t * ZTyp.t * Exp.t
  | ZLamE of Ingraph.t * Pat.t * Typ.t * t
  | ZAppE1 of Ingraph.t * t * Exp.t
  | ZAppE2 of Ingraph.t * Exp.t * t
  | ZPlusE1 of Ingraph.t * t * Exp.t
  | ZPlusE2 of Ingraph.t * Exp.t * t
  | ZTimesE1 of Ingraph.t * t * Exp.t
  | ZTimesE2 of Ingraph.t * Exp.t * t
  | ZConflict of t * Exp.C.t

let rec erase_cursor : t -> Exp.t = function
  | Cursor e -> e
  | ZLamP (ingraph, zpat, ty, body) ->
      Lam (ingraph, ZPat.erase_cursor zpat, ty, body)
  | ZLamT (ingraph, pat, zty, body) ->
      Lam (ingraph, pat, ZTyp.erase_cursor zty, body)
  | ZLamE (ingraph, pat, ty, zbody) -> Lam (ingraph, pat, ty, erase_cursor zbody)
  | ZAppE1 (ingraph, ze1, e2) -> App (ingraph, erase_cursor ze1, e2)
  | ZAppE2 (ingraph, e1, ze2) -> App (ingraph, e1, erase_cursor ze2)
  | ZPlusE1 (ingraph, ze1, e2) -> Plus (ingraph, erase_cursor ze1, e2)
  | ZPlusE2 (ingraph, e1, ze2) -> Plus (ingraph, e1, erase_cursor ze2)
  | ZTimesE1 (ingraph, ze1, e2) -> Times (ingraph, erase_cursor ze1, e2)
  | ZTimesE2 (ingraph, e1, ze2) -> Times (ingraph, e1, erase_cursor ze2)
  | ZConflict (ze, conflict) ->
      Exp.Conflict (Exp.C.add (erase_cursor ze) conflict)
