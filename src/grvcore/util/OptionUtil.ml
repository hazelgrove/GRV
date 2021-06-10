let map f = function None -> None | Some x -> Some (f x)

let bind o f = match o with Some x -> f x | None -> None

let product o1 o2 =
  match (o1, o2) with Some x, Some y -> Some (x, y) | _ -> None

module Syntax = struct
  let ( let+ ) x f = map f x

  let ( and+ ) o1 o2 = product o1 o2

  let ( let* ) x f = bind x f
end
