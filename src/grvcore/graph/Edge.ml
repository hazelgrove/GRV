type t' = { source : Cursor.t; target : Vertex.t } [@@deriving sexp, show]

type t = t' Uuid.Wrap.t [@@deriving sexp, show]

let show (edge : t) : string =
  Uuid.Id.show edge.value.source.vertex.id
  ^ " -> "
  ^ Uuid.Id.show edge.value.target.id

let mk (source : Cursor.t) (target : Vertex.t) : t =
  Uuid.Wrap.mk { source; target }

let compare : t -> t -> int = Uuid.Wrap.compare

let equal (edge1 : t) (edge2 : t) : bool = compare edge1 edge2 = 0

let source (edge : t) : Cursor.t = (Uuid.Wrap.unmk edge).source

let target (edge : t) : Vertex.t = (Uuid.Wrap.unmk edge).target

module OrderedType = struct
  type nonrec t = t

  let compare = compare
end

module Map = Map.Make (OrderedType)
module Set = Set.Make (OrderedType)

let print_set (edges : Set.t) : unit =
  Format.(
    printf "[";
    Set.iter
      (fun e ->
        printf "%s -> %s; "
          (Uuid.Id.show e.value.source.vertex.id)
          (Uuid.Id.show e.value.target.id))
      edges;
    printf "]")

let partition_set (edges : Set.t) (pivot : Vertex.t) : Set.t * Set.t =
  Set.partition (fun (e : t) -> e.value.source.vertex = pivot) edges

let concat_sets (sets : Set.t list) : Set.t =
  List.map Set.elements sets |> List.concat |> Set.of_list
