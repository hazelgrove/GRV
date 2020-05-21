type t = { vertex : Vertex.t; index : Lang.Index.t } [@@deriving ord]

let mk (vertex : Vertex.t) (index : Lang.Index.t) : t = { vertex; index }

let root = { vertex = Vertex.root; index = Root_root_root }

let pp (fmt : Format.formatter) (cursor : t) : unit =
  Format.fprintf fmt "%a/%a" Uuid.Id.pp cursor.vertex.id Lang.Index.pp
    cursor.index

module OrderedType = struct
  type nonrec t = t

  let compare = compare
end

module Map = Map.Make (OrderedType)
module Set = Set.Make (OrderedType)
