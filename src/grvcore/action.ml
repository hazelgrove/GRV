(* TODO: factor App and other ast constructor insertions into a constructor *)
type t = App | MoveIn [@@deriving sexp_of]
