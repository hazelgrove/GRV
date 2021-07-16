open OptionUtil.Syntax

type t = ZExp of ZExp.t | ZPat of ZPat.t | ZTyp of ZTyp.t [@@deriving sexp]

let reduce (f : ZExp.t -> 'a) (g : ZPat.t -> 'a) (h : ZTyp.t -> 'a) (zterm : t)
    : 'a =
  match zterm with
  | ZExp zexp -> f zexp
  | ZPat zpat -> g zpat
  | ZTyp ztyp -> h ztyp

let map (f : ZExp.t -> Exp.t) (g : ZPat.t -> Pat.t) (h : ZTyp.t -> Typ.t)
    (zterm : t) : Term.t =
  match zterm with
  | ZExp zexp -> Exp (f zexp)
  | ZPat zpat -> Pat (g zpat)
  | ZTyp ztyp -> Typ (h ztyp)

let cursor_position : t -> GroveLang.Position.t option =
  reduce ZExp.cursor_position ZPat.cursor_position ZTyp.cursor_position

let cursor_sort : t -> GroveLang.Sort.t =
  reduce ZExp.cursor_sort ZPat.cursor_sort ZTyp.cursor_sort

let cursor_term : t -> Term.t =
  map ZExp.erase_cursor ZPat.erase_cursor ZTyp.erase_cursor

let erase_cursor : t -> Term.t = cursor_term

let id : t -> Vertex.id option = reduce ZExp.id ZPat.id ZTyp.id

let recomp : t -> Graph.t * GraphCursor.t =
  reduce ZExp.recomp ZPat.recomp ZTyp.recomp

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

  let cursor_sort ((_, zterm) : t) : GroveLang.Sort.t = cursor_sort zterm

  let cursor_term ((_, zterm) : t) : Term.t = cursor_term zterm

  let erase_cursor ((terms, zterm) : t) : Term.Set.t =
    Term.Set.add (erase_cursor zterm) terms

  let recomp ((terms, zterm) : t) : Graph.t * GraphCursor.t =
    let zterm_graph, graph_cursor = recomp zterm in
    let terms_graph = Term.Set.recomp terms in
    let graph = Graph.union2 zterm_graph terms_graph in
    (graph, graph_cursor)

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
