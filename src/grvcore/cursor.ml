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

let rec unwind (side : side) (cursor : t) : t =
  match cursor with
  | Here -> Here
  | To (_, Here) -> To (side, Here)
  | To (side', cursor') -> To (side', unwind side cursor')

let rec last_side_of : t -> side option = function
  | Here -> None
  | To (side, Here) -> Some side
  | To (_, cursor') -> last_side_of cursor'
