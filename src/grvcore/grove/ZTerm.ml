open OptionUtil.Syntax

type t = ZExp of ZExp.t | ZPat of ZPat.t | ZTyp of ZTyp.t [@@deriving sexp]

let cursor_position : t -> GroveLang.Position.t option = function
  | ZExp zexp -> ZExp.cursor_position zexp
  | ZPat zpat -> ZPat.cursor_position zpat
  | ZTyp ztyp -> ZTyp.cursor_position ztyp

let erase_cursor : t -> Term.t = function
  | ZExp zexp -> Exp (ZExp.erase_cursor zexp)
  | ZPat zpat -> Pat (ZPat.erase_cursor zpat)
  | ZTyp zty -> Typ (ZTyp.erase_cursor zty)

(* let follow_cursor : t -> GroveLang.position list = function
  | ZExp zexp -> ZExp.follow_cursor zexp
  | ZPat zpat -> ZPat.follow_cursor zpat
  | ZTyp ztyp -> ZTyp.follow_cursor ztyp *)

(* let place_cursor (path : GroveLang.position list) (term : Term.t) : t =
  match term with
  | Exp zexp -> ZExp.place_cursor path zexp
  | Pat zpat -> ZPat.place_cursor path zpat
  | Typ ztyp -> ZTyp.place_cursor path ztyp *)

let id : t -> Vertex.id option = function
  | ZExp zexp -> ZExp.id zexp
  | ZPat zpat -> ZPat.id zpat
  | ZTyp ztyp -> ZTyp.id ztyp

(* let move (move_action : UserAction.move) (zterm : t) : t option =
  match zterm with
  | ZExp zexp ->
      let+ zexp = ZExp.move move_action zexp in
      ZExp zexp
  | ZPat zpat ->
      let+ zpat = ZPat.move move_action zpat in
      ZPat zpat
  | ZTyp ztyp ->
      let+ ztyp = ZTyp.move move_action ztyp in
      ZTyp ztyp *)

(* let edit (edit_action : UserAction.edit) (zterm : t) (u_gen : Id.Gen.t) :
    (GraphAction.t list * Id.Gen.t) option =
  match zterm with
  | ZExp zexp -> ZExp.edit edit_action zexp u_gen
  | ZPat zpat -> ZPat.edit edit_action zpat u_gen
  | ZTyp zty -> ZTyp.edit edit_action zty u_gen *)

module Root = struct
  type nonrec t = Root of Term.t | ZRoot of t
end

module Set = struct
  open Sexplib

  type elt = t

  type nonrec t = Term.Set.t * t

  let sexp_of_t (zset : t) : Sexp.t =
    Sexplib0.Sexp_conv.sexp_of_pair Term.Set.sexp_of_t sexp_of_t zset

  let t_of_sexp (sexp : Sexp.t) : t =
    Sexplib0.Sexp_conv.pair_of_sexp Term.Set.t_of_sexp t_of_sexp sexp

  let singleton (zterm : elt) : t = (Term.Set.empty, zterm)

  let find_root : t -> Root.t option = function
    | _, (ZPat _ | ZTyp _) -> None
    | terms, (ZExp zexp as zterm) ->
        if ZExp.is_root zexp then Some (ZRoot zterm)
        else
          let+ root = Term.Set.find_root terms in
          Root.Root root

  let cursor_position ((_, zterm) : t) : GroveLang.Position.t option =
    cursor_position zterm

  let erase_cursor ((terms, zterm) : t) : Term.Set.t =
    Term.Set.add (erase_cursor zterm) terms

  (* let follow_cursor ((_, zterm) : t) :
       Vertex.id option * GroveLang.position list =
     (id zterm, follow_cursor zterm) *)

  (* let place_cursor ((id_opt, path) : Vertex.id option * GroveLang.position list)
       (terms : Term.Set.t) : t option =
     let* id = id_opt in
     let is_root term = Term.id term = id in
     match Term.Set.elements terms |> List.find_opt is_root with
     | Some term ->
         let terms = Term.Set.remove term terms in
         let+ zterm = place_cursor path term in
         (terms, zterm) *)

  (* let move (move_action : UserAction.move) ((set, zterm) : t) : t option =
     let+ zterm = move move_action zterm in
     (set, zterm) *)

  (* let edit (edit_action : UserAction.edit) ((_, zterm) : t) (u_gen : Id.Gen.t) :
       (GraphAction.t list * Id.Gen.t) option =
     edit edit_action zterm u_gen *)
end
