open OptionUtil.Syntax

module Make (T : sig
  type t

  val compare : t -> t -> int

  val constructor : t -> GroveLang.constructor
end) =
struct
  module Conflict = Conflict.Make (T)

  type t =
    | T of Ingraph.t * T.t
    | MultiparentRef of Edge.t
    | UnicycleRef of Edge.t
    | Conflict of Conflict.t
    | Hole

  let ingraph : t -> Ingraph.t option = function
    | T (ingraph, _) -> Some ingraph
    | MultiparentRef edge | UnicycleRef edge ->
        Some (Ingraph.singleton edge Plus)
    | Conflict _ | Hole -> None

  let constructor : t -> GroveLang.constructor option = function
    | T (_, term) -> Some (T.constructor term)
    | (MultiparentRef _ | UnicycleRef _) as term ->
        let+ ingraph = ingraph term in
        ingraph.invertex.constructor
    | Conflict _ | Hole -> None
end
