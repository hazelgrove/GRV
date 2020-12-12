type t' = { source : Cursor.t; target : Vertex.t } [@@deriving sexp]

type t = t' Uuid.wrap [@@deriving sexp]

let mk (source : Cursor.t) (target : Vertex.t) : t =
  Uuid.wrap { source; target }

module OrderedType = struct
  type nonrec t = t

  let compare = Uuid.compare
end

module Map = Map.Make (OrderedType)
module Set = Set.Make (OrderedType)

let partition_set (edges : Set.t) (pivot : Vertex.t) : Set.t * Set.t =
  Set.partition (fun edge -> edge.value.source.vertex = pivot) edges

let union_sets (sets : Set.t list) : Set.t =
  List.map Set.elements sets |> List.concat |> Set.of_list
