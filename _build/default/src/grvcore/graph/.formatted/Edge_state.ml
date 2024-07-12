type t = Created | Deleted [@@deriving sexp]

let to_string : t -> string = function Created -> "+" | Deleted -> "-"
