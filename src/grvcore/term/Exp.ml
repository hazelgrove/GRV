module T = struct
  type t =
    | Var of Graph.t
    | Lam of Graph.t * Pat.t * Typ.t * t
    | App of Graph.t

  let compare = compare
end

module Conflict = Conflict.Make (T)

type t = Exp of T.t | ExpConflict of Conflict.t
