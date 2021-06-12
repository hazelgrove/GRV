let map f = function None -> None | Some x -> Some (f x)

let bind o f = match o with Some x -> f x | None -> None

let product o1 o2 =
  match (o1, o2) with Some x, Some y -> Some (x, y) | _ -> None

module Syntax = struct
  let ( let+ ) x f = map f x

  let ( and+ ) o1 o2 = product o1 o2

  let ( let* ) x f = bind x f
end

open Syntax

let rec of_list : 'a option list -> 'a list option = function
  | [] -> Some []
  | hd_opt :: opt_tl ->
      let* hd = hd_opt in
      let+ tl = of_list opt_tl in
      hd :: tl
