module T = struct
  type t = Var of Graph.t * string

  let compare = compare
end

module Conflict = Conflict.Make (T)

type t = Pat of T.t | PatConflict of Conflict.t
