type t = { vertex : Vertex.t; index : Lang.Index.t }

let mk (vertex : Vertex.t) (index : Lang.Index.t) : t = { vertex; index }

let root = { vertex = Vertex.root; index = Root_root_root }

let compare (c1 : t) (c2 : t) : int =
  match Vertex.compare c1.vertex c2.vertex with
  | 0 -> Lang.Index.compare c1.index c2.index
  | i -> i

let pp (fmt : Format.formatter) (cursor : t) : unit =
  Format.fprintf fmt "%a/%a" Uuid.Id.pp cursor.vertex.id Lang.Index.pp
    cursor.index

module OrderedType = struct
  type nonrec t = t

  let compare = compare
end

module Map = Map.Make (OrderedType)
