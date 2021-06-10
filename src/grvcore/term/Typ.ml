module T = struct
  type t = Arrow of Graph.t * t * t | Num of Graph.t

  let compare = compare

  let constructor : t -> GroveLang.constructor = function
    | Arrow (_, _, _) -> TypArrow
    | Num _ -> TypNum
end

include Sort.Make (T)
