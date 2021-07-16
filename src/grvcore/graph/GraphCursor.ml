type t =
  | OnVertex of Vertex.id
  | OnRef of Vertex.id * GroveLang.Position.t
  | OnHole of Vertex.id * GroveLang.Position.t
  | OnConflict of Vertex.id * GroveLang.Position.t
  | InConflict of Vertex.id * GroveLang.Position.t * Vertex.id
