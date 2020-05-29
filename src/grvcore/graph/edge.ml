type t = { source : Cursor.t; target : Vertex.t }
[@@deriving ord, sexp_of, show]

let mk (source : Cursor.t) (target : Vertex.t) : t = { source; target }

module OrderedType = struct
  type nonrec t = t

  let compare = compare
end

module Map = Map.Make (OrderedType)
module Set = Set.Make (OrderedType)
