type t = Created | Destroyed [@@deriving sexp]

let to_string : t -> string = function Created -> "+" | Destroyed -> "-"
