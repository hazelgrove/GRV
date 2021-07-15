type t = Exp of Exp.t | Pat of Pat.t | Typ of Typ.t [@@deriving sexp]

let is_root : t -> bool = function
  | Pat _ | Typ _ -> false
  | Exp exp -> Exp.is_root exp

let id : t -> Id.t option = function
  | Exp exp -> Exp.id exp
  | Pat pat -> Pat.id pat
  | Typ typ -> Typ.id typ

let ingraph : t -> Ingraph.t option = function
  | Exp exp -> Exp.ingraph exp
  | Pat pat -> Pat.ingraph pat
  | Typ typ -> Typ.ingraph typ

module OrderedType = struct
  type nonrec t = t

  let compare = compare
end

module Set = struct
  open Sexplib
  include Set.Make (OrderedType)

  let sexp_of_t (set : t) : Sexp.t = elements set |> Std.sexp_of_list sexp_of_t

  let t_of_sexp (sexp : Sexp.t) : t = Std.list_of_sexp t_of_sexp sexp |> of_list

  let find_root (terms : t) : elt option = find_first_opt is_root terms
end
