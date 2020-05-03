module Edge = struct
  type t = t' Uuid.t [@@deriving show]

  and t' = { source : Vertex.t; index : Index.t; target : Vertex.t }

  type state = Created | Destroyed [@@deriving show]

  let mk (source : Vertex.t) (index : Index.t) (target : Vertex.t) : t =
    let e' : t' = { source; index; target } in
    Uuid.wrap e'

  let compare : t -> t -> int = Uuid.compare

  let equal (edge1 : t) (edge2 : t) : bool = compare edge1 edge2 = 0

  let source (edge : t) : Vertex.t = (Uuid.unwrap edge).source

  let target (edge : t) : Vertex.t = (Uuid.unwrap edge).target

  (* TODO rearrange edge field accessors *)
  let index (edge : t) : Index.t = (Uuid.unwrap edge).index
end

include Edge
module Map = Map.Make (Edge)

module Set = Set.Make (struct
  type t = Edge.t

  let compare : t -> t -> int = Edge.compare
end)
