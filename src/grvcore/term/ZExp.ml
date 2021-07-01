open OptionUtil.Syntax

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
[@@deriving sexp]

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

(* let rec follow_cursor : t -> Term.t = function
  | Cursor exp -> Exp exp
  | ZLamP (_, zpat, _, _) -> ZPat.follow_cursor zpat
  | ZLamT (_, _, ztyp, _) -> ZTyp.follow_cursor ztyp
  | ZLamE (_, _, _, zexp)
  | ZAppE1 (_, zexp, _)
  | ZAppE2 (_, _, zexp)
  | ZPlusE1 (_, zexp, _)
  | ZPlusE2 (_, _, zexp)
  | ZTimesE1 (_, zexp, _)
  | ZTimesE2 (_, _, zexp)
  | ZConflict (zexp, _) ->
      follow_cursor zexp *)

let rec move (move_action : UserAction.move) (zexp : t) : t option =
  match move_action with
  | Up -> (
      match zexp with
      | Cursor _ -> None
      | ZLamP (ingraph, Cursor pat, typ, exp)
      | ZLamT (ingraph, pat, Cursor typ, exp)
      | ZLamE (ingraph, pat, typ, Cursor exp) ->
          Some (Cursor (Lam (ingraph, pat, typ, exp)))
      | ZLamP (ingraph, zpat, typ, exp) ->
          let+ zpat = ZPat.move move_action zpat in
          ZLamP (ingraph, zpat, typ, exp)
      | ZLamT (ingraph, pat, ztyp, exp) ->
          let+ ztyp = ZTyp.move move_action ztyp in
          ZLamT (ingraph, pat, ztyp, exp)
      | ZLamE (ingraph, pat, typ, zexp) ->
          let+ zexp = move move_action zexp in
          ZLamE (ingraph, pat, typ, zexp)
      | ZAppE1 (_, _, _) -> ( ??)
      | ZAppE2 (_, _, _) -> ( ??)
      | ZPlusE1 (_, _, _) -> ( ??)
      | ZPlusE2 (_, _, _) -> ( ??)
      | ZTimesE1 (_, _, _) -> ( ??)
      | ZTimesE2 (_, _, _) -> ( ??)
      | ZConflict (_, _) -> ( ??))
  | Down -> ( ??)
  | Left -> ( ??)
  | Right -> ( ??)
  | Select _ -> ( ??)

let rec edit (edit_action : UserAction.edit) (zexp : t) (u_gen : Id.Gen.t) :
    (GraphAction.t list * Id.Gen.t) option =
  match zexp with
  | Cursor exp -> Exp.edit edit_action exp u_gen
  | ZLamP (_, zpat, _, _) -> ZPat.edit edit_action zpat u_gen
  | ZLamT (_, _, zty, _) -> ZTyp.edit edit_action zty u_gen
  | ZLamE (_, _, _, zexp)
  | ZAppE1 (_, zexp, _)
  | ZAppE2 (_, _, zexp)
  | ZPlusE1 (_, zexp, _)
  | ZPlusE2 (_, _, zexp)
  | ZTimesE1 (_, zexp, _)
  | ZTimesE2 (_, _, zexp)
  | ZConflict (zexp, _) ->
      edit edit_action zexp u_gen
