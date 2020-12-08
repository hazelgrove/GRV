type t = { vertex : Vertex.t; index : Lang.Index.t } [@@deriving sexp]

let to_string (cursor : t) : string =
  Uuid.Id.to_string cursor.vertex.id ^ "." ^ Lang.Index.short_name cursor.index

let root = { vertex = Vertex.root; index = Root_root_root }

module OrderedType = struct
  type nonrec t = t

  let compare = compare
end

module Map = Map.Make (OrderedType)
module Set = Set.Make (OrderedType)
