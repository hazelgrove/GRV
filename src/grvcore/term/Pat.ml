module rec T : sig
  type t =
    | Var of Ingraph.t * string
    | Multiparent of Edge.t
    | Unicycle of Edge.t
    | Conflict of C.t
    | Hole

  val compare : t -> t -> int
end = struct
  type t =
    | Var of Ingraph.t * string
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
  | Var (_, name) -> Some (PatVar name)
  | Multiparent _ | Unicycle _ | Conflict _ | Hole -> None

let ingraph : t -> Ingraph.t option = function
  | Var (ingraph, _) -> Some ingraph
  | Multiparent edge | Unicycle edge -> Some (Ingraph.of_edge edge)
  | Conflict _ | Hole -> None
