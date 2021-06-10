module T = struct
  type t = Arrow of Graph.t * t * t | Num of Graph.t

  let compare = compare
end

module Conflict = Conflict.Make (T)

type t = Typ of T.t | TypConflict of Conflict.t
