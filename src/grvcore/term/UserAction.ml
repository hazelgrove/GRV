type edit =
  | Construct of GroveLang.constructor
  | Delete
  | Relocate of Vertex.t * GroveLang.position
[@@deriving sexp]

type move = Left | Right | Up | Down | Select of Vertex.id [@@deriving sexp]
