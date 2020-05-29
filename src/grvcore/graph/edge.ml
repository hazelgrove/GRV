type t = { source : Cursor.t; target : Vertex.t }
[@@deriving ord, sexp_of, show]

let mk (source : Cursor.t) (target : Vertex.t) : t = { source; target }

module OrderedType = struct
  type nonrec t = t

  let compare e1 e2 = Vertex.compare e2.target e1.target
end

module Map = Map.Make (OrderedType)
module Set = Set.Make (OrderedType)
