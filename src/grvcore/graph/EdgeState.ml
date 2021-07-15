type t = Plus | Minus [@@deriving sexp]

let to_string : t -> string = function Plus -> "+" | Minus -> "-"
