type t =
  | Construct of GroveLang.constructor
  | Delete
  | Reposition of Vertex.t * GroveLang.position
