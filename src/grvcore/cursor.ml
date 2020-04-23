type t = Here | To of side * t

and side = Left | Right

let rec push side cursor =
  match cursor with
  | Here -> To (side, Here)
  | To (side', cursor') -> To (side', push side cursor')

let rec pop cursor =
  match cursor with
  | Here | To (_, Here) -> Here
  | To (side', cursor') -> To (side', pop cursor')

let rec look_to side cursor =
  match cursor with
  | Here -> Here
  | To (_, Here) -> To (side, Here)
  | To (side', cursor') -> To (side', look_to side cursor')

let rec last_side_of : t -> side option = function
  | Here -> None
  | To (side, Here) -> Some side
  | To (_, cursor') -> last_side_of cursor'
