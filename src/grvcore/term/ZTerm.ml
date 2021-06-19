type t = ZExp of ZExp.t | ZPat of ZPat.t | ZTyp of ZTyp.t

let erase_cursor : t -> Term.t = function
  | ZExp zexp -> Exp (ZExp.erase_cursor zexp)
  | ZPat zpat -> Pat (ZPat.erase_cursor zpat)
  | ZTyp zty -> Typ (ZTyp.erase_cursor zty)

let apply_action (action : UserAction.t) (zterm : t) (u_gen : Id.Gen.t) :
    (GraphAction.t list * Id.Gen.t) option =
  match zterm with
  | ZExp zexp -> ZExp.apply_action action zexp u_gen
  | ZPat zpat -> ZPat.apply_action action zpat u_gen
  | ZTyp zty -> ZTyp.apply_action action zty u_gen

module Set = struct
  type nonrec t = Term.Set.t * t

  let erase_cursor ((terms, zterm) : t) : Term.Set.t =
    Term.Set.add (erase_cursor zterm) terms

  let apply_action (action : UserAction.t) (zterms : t) (u_gen : Id.Gen.t) :
      (GraphAction.t list * Id.Gen.t) option =
    apply_action action (snd zterms) u_gen
end
