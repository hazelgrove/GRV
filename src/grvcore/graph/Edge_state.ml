type t = Created | Destroyed [@@deriving sexp]

let pp (fmt : Format.formatter) (state : t) : unit =
  Format.fprintf fmt (match state with Created -> "+" | Destroyed -> "-")
