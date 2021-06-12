module rec T : sig
  type t =
    | Var of Ingraph.t * string
    | Lam of Ingraph.t * Pat.t * Typ.t * t
    | App of Ingraph.t * t * t
    | Num of Ingraph.t * int
    | Plus of Ingraph.t * t * t
    | Times of Ingraph.t * t * t
    | Multiparent of Edge.t
    | Unicycle of Edge.t
    | Conflict of C.t
    | Hole

  val compare : t -> t -> int
end = struct
  type t =
    | Var of Ingraph.t * string
    | Lam of Ingraph.t * Pat.t * Typ.t * t
    | App of Ingraph.t * t * t
    | Num of Ingraph.t * int
    | Plus of Ingraph.t * t * t
    | Times of Ingraph.t * t * t
    | Multiparent of Edge.t
    | Unicycle of Edge.t
    | Conflict of C.t
    | Hole

  let compare = compare
end

and C : sig
  type t

  val of_list : T.t list -> t
end =
  Set.Make (T)

include T

let constructor : t -> GroveLang.constructor option = function
  | Var (_, name) -> Some (ExpVar name)
  | Lam (_, _, _, _) -> Some ExpLam
  | App (_, _, _) -> Some ExpApp
  | Num (_, n) -> Some (ExpNum n)
  | Plus (_, _, _) -> Some ExpPlus
  | Times (_, _, _) -> Some ExpTimes
  | Multiparent _ | Unicycle _ | Conflict _ | Hole -> None

let ingraph : t -> Ingraph.t option = function
  | Var (ingraph, _)
  | Lam (ingraph, _, _, _)
  | App (ingraph, _, _)
  | Num (ingraph, _)
  | Plus (ingraph, _, _)
  | Times (ingraph, _, _) ->
      Some ingraph
  | Multiparent edge | Unicycle edge -> Some (Ingraph.of_edge edge)
  | Conflict _ | Hole -> None
