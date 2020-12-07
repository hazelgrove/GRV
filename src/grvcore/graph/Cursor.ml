type t = { vertex : Vertex.t; index : Lang.Index.t }
[@@deriving ord, sexp, show]

let root = { vertex = Vertex.root; index = Root_root_root }

module OrderedType = struct
  type nonrec t = t

  let compare = compare
end

module Map = Map.Make (OrderedType)
module Set = Set.Make (OrderedType)
