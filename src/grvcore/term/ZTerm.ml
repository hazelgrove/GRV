open OptionUtil.Syntax

type t = ZExp of ZExp.t | ZPat of ZPat.t | ZTyp of ZTyp.t [@@deriving sexp]

let erase_cursor : t -> Term.t = function
  | ZExp zexp -> Exp (ZExp.erase_cursor zexp)
  | ZPat zpat -> Pat (ZPat.erase_cursor zpat)
  | ZTyp zty -> Typ (ZTyp.erase_cursor zty)

(* let follow_cursor : t -> Term.t = function
  | ZExp zexp -> ZExp.follow_cursor zexp
  | ZPat zpat -> ZPat.follow_cursor zpat
  | ZTyp ztyp -> ZTyp.follow_cursor ztyp *)

let move (move_action : UserAction.move) (zterm : t) : t option =
  match zterm with
  | ZExp zexp ->
      let+ zexp = ZExp.move move_action zexp in
      ZExp zexp
  | ZPat _ ->
      let+ zpat = ZPat.move move_action zpat in
      ZPat zpat
  | ZTyp _ ->
      let+ ztyp = ZTyp.move move_action ztyp in
      ZTyp ztyp

let edit (edit_action : UserAction.edit) (zterm : t) (u_gen : Id.Gen.t) :
    (GraphAction.t list * Id.Gen.t) option =
  match zterm with
  | ZExp zexp -> ZExp.edit edit_action zexp u_gen
  | ZPat zpat -> ZPat.edit edit_action zpat u_gen
  | ZTyp zty -> ZTyp.edit edit_action zty u_gen

module Set = struct
  open Sexplib

  type elt = t

  type nonrec t = Term.Set.t * t

  let sexp_of_t (zset : t) : Sexp.t =
    Sexplib0.Sexp_conv.sexp_of_pair Term.Set.sexp_of_t sexp_of_t zset

  let t_of_sexp (sexp : Sexp.t) : t =
    Sexplib0.Sexp_conv.pair_of_sexp Term.Set.t_of_sexp t_of_sexp sexp

  let singleton (zterm : elt) : t = (Term.Set.empty, zterm)

  let erase_cursor ((terms, zterm) : t) : Term.Set.t =
    Term.Set.add (erase_cursor zterm) terms

  (* let follow_cursor ((_, zterm) : t) : Term.t = follow_cursor zterm *)

  (* let move (move_action : UserAction.move) ((set, zterm) : t) : t option =
     let* zterm = move *)
end
