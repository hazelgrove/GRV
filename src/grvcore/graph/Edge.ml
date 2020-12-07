type t' = { source : Cursor.t; target : Vertex.t } [@@deriving sexp]

type t = t' Uuid.Wrap.t [@@deriving sexp]

let mk (source : Cursor.t) (target : Vertex.t) : t =
  Uuid.Wrap.mk { source; target }

module OrderedType = struct
  type nonrec t = t

  let compare = Uuid.Wrap.compare
end

module Map = Map.Make (OrderedType)
module Set = Set.Make (OrderedType)

let partition_set (edges : Set.t) (pivot : Vertex.t) : Set.t * Set.t =
  Set.partition (fun (e : t) -> e.value.source.vertex = pivot) edges

let concat_sets (sets : Set.t list) : Set.t =
  List.map Set.elements sets |> List.concat |> Set.of_list

let children_in_set (edges : Set.t) (vertex : Vertex.t) : Set.t =
  Set.filter (fun edge -> edge.value.source.vertex = vertex) edges
