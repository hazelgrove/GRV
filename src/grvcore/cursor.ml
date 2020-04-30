type t = Here | To of side * t

and side = Left | Right

let rec push (side : side) (cursor : t) : t =
  match cursor with
  | Here -> To (side, Here)
  | To (side', cursor') -> To (side', push side cursor')

let rec pop (cursor : t) : t =
  match cursor with
  | Here | To (_, Here) -> Here
  | To (side', cursor') -> To (side', pop cursor')

let rec unwind (cursor : t) (side : side) : t =
  match cursor with
  | Here -> Here
  | To (_, Here) -> To (side, Here)
  | To (side', cursor') -> To (side', unwind cursor' side)

let rec last : t -> t = function
  | (To (_, Here) | Here) as cursor -> cursor
  | To (_, cursor') -> last cursor'
