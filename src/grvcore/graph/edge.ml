type t' = { source : Vertex.t; index : Index.t; target : Vertex.t }
[@@deriving show]

type t = t' Uuid.Wrap.t [@@deriving show]

let mk (source : Vertex.t) (index : Index.t) (target : Vertex.t) : t =
  Uuid.Wrap.mk { source; index; target }

let compare : t -> t -> int = Uuid.Wrap.compare

let equal (edge1 : t) (edge2 : t) : bool = compare edge1 edge2 = 0

let source (edge : t) : Vertex.t = (Uuid.Wrap.unmk edge).source

(* TODO rearrange edge field accessors *)
let index (edge : t) : Index.t = (Uuid.Wrap.unmk edge).index

let target (edge : t) : Vertex.t = (Uuid.Wrap.unmk edge).target

module OrderedType = struct
  type nonrec t = t

  let compare = compare
end

module Map = Map.Make (OrderedType)
module Set = Set.Make (OrderedType)
