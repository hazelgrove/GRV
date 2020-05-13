type t = { parent : Vertex.t; index : Index.t }

let mk (parent : Vertex.t) (index : Index.t) : t = { parent; index }

let root = { parent = Vertex.root; index = Root_root_root }

let compare (child1 : t) (child2 : t) : int =
  match Vertex.compare child1.parent child2.parent with
  | 0 -> Index.compare child1.index child2.index
  | i -> i

let pp (fmt : Format.formatter) (child : t) : unit =
  Format.fprintf fmt "%a/%a" Uuid.Id.pp child.parent.id Index.pp child.index

module OrderedType = struct
  type nonrec t = t

  let compare = compare
end

module Map = Map.Make (OrderedType)
