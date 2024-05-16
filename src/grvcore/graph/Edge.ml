type t' = { source : Cursor.t; target : Vertex.t } [@@deriving sexp]
type t = t' Uuid.wrap [@@deriving sexp]

(* Wraps edge with unique identifier *)
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

(* String Conversions *)

let to_string (edge : t) : string =
  Vertex.to_string edge.value.source.vertex
  ^ ":"
  ^ Lang.Position.short_name edge.value.source.position
  ^ " -> "
  ^ Vertex.to_string edge.value.target

let set_to_string (edges : Set.t) : string =
  "{"
  ^ (Set.fold (fun edge strs -> to_string edge :: strs) edges []
    |> List.rev |> String.concat ", ")
  ^ "}"

(* Tests for edge behavior *)
let%test "first edge: " =
  let root = Vertex.root in
  let root_cursor = Cursor.root in
  let _ = Cursor.root |> Cursor.sexp_of_t |> Util.Sexp.print in
  let edge = mk root_cursor root in
  let edge_str = to_string edge in
  let _ = edge.id |> Uuid.Id.to_string |> print_endline in
  edge_str = "0:root -> 0"
  && edge.value.source = root_cursor
  && edge.value.target = root
  && Uuid.Id.to_string edge.id = "1"
  &&
  let _ = "Edge string: " ^ edge_str |> print_endline in
  true
  &&
  let _ = root |> Vertex.to_string |> print_endline in
  true
