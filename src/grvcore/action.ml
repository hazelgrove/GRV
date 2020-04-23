(* TODO: factor App and other ast constructor insertions into a constructor *)
type t = App | Move of direction [@@deriving sexp_of]

and direction = In | Out | Left | Right [@@deriving sexp_of]
