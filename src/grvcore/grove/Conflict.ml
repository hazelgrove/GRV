open OptionUtil.Syntax

module type S = sig
  open Sexplib

  include Set.S

  val sexp_of_t : t -> Sexp.t

  val t_of_sexp : Sexp.t -> t

  val construct_map :
    (elt -> Id.Gen.t -> (GraphAction.t list * Id.Gen.t) option) ->
    t ->
    Id.Gen.t ->
    (GraphAction.t list * Id.Gen.t) option
end

module type SexpableOrderedType = sig
  open Sexplib

  include Set.OrderedType

  val sexp_of_t : t -> Sexp.t

  val t_of_sexp : Sexp.t -> t
end

module Make (T : SexpableOrderedType) = struct
  open Sexplib
  include Set.Make (T)

  let sexp_of_t (conflict : t) : Sexp.t =
    elements conflict |> Std.sexp_of_list T.sexp_of_t

  let t_of_sexp (sexp : Sexp.t) : t =
    Std.list_of_sexp T.t_of_sexp sexp |> of_list

  let construct_map
      (f : elt -> Id.Gen.t -> (GraphAction.t list * Id.Gen.t) option)
      (conflict : t) (u_gen : Id.Gen.t) : (GraphAction.t list * Id.Gen.t) option
      =
    elements conflict
    |> List.fold_left
         (fun opt elt ->
           let* acc, u_gen = opt in
           let+ acc', u_gen = f elt u_gen in
           (acc @ acc', u_gen))
         (Some ([], u_gen))
end
