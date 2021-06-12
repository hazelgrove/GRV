module rec T : sig
  type t =
    | Arrow of Ingraph.t * t * t
    | Num of Ingraph.t
    | Multiparent of Edge.t
    | Unicycle of Edge.t
    | Conflict of C.t
    | Hole

  val compare : t -> t -> int
end = struct
  type t =
    | Arrow of Ingraph.t * t * t
    | Num of Ingraph.t
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
  | Arrow (_, _, _) -> Some TypArrow
  | Num _ -> Some TypNum
  | Multiparent _ | Unicycle _ | Conflict _ | Hole -> None

let ingraph : t -> Ingraph.t option = function
  | Arrow (ingraph, _, _) | Num ingraph -> Some ingraph
  | Multiparent edge | Unicycle edge -> Some (Ingraph.of_edge edge)
  | Conflict _ | Hole -> None
