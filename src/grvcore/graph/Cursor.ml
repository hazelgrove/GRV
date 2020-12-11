type t = { vertex : Vertex.t; position : Lang.Position.t } [@@deriving sexp]

let to_string (cursor : t) : string =
  Format.sprintf "%s.%s"
    (Uuid.Id.to_string cursor.vertex.id)
    (Lang.Position.short_name cursor.position)

let root : t = { vertex = Vertex.root; position = Root_root_root }

module OrderedType = struct
  type nonrec t = t

  let compare = compare
end

module Map = Map.Make (OrderedType)
module Set = Set.Make (OrderedType)
