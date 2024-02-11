type t = Lang.Constructor.t Uuid.wrap [@@deriving sexp]

let mk : Lang.Constructor.t -> t = Uuid.wrap
let root : t = Uuid.well_known 0 Lang.Constructor.Root_root

module OrderedType = struct
  type nonrec t = t

  let compare = Uuid.compare
end

module Map = Map.Make (OrderedType)
module Set = Set.Make (OrderedType)

(* String Conversions *)

let to_string (vertex : t) : string = Uuid.Id.to_string vertex.id

let set_to_string (vertexes : Set.t) : string =
  "{"
  ^ (Set.fold (fun vertex strs -> to_string vertex :: strs) vertexes []
    |> List.rev |> String.concat ", ")
  ^ "}"

(*******************************************************************************
 * Unit Tests
 ******************************************************************************)

(* Some checks on our assumptions about how (vertex) sets work. *)

let%test "empty set has cardinality 0" = Set.(cardinal empty) = 0
let%test "empty set has no elements" = Set.(elements empty) = []

let%test "singleton set has cardinality 1" =
  Set.(empty |> add (mk Exp_app) |> cardinal) = 1

let%test "singleton set has one element" =
  let v = mk Exp_app in
  Set.(empty |> add v |> elements) = [ v ]

let%test "adding the same element twice produces a singleton set" =
  let v = mk Exp_app in
  let vs = Set.(empty |> add v |> add v) in
  Set.cardinal vs = 1 && Set.elements vs = [ v ]

let%test "adding the same element twice produces a singleton set" =
  let v = mk Exp_app in
  let vs = Set.(empty |> add v |> add v) in
  Set.cardinal vs = 1 && Set.elements vs = [ v ]

let%test "adding the same element thrice produces a singleton set" =
  let v = mk Exp_app in
  let vs = Set.(empty |> add v |> add v |> add v) in
  Set.cardinal vs = 1 && Set.elements vs = [ v ]

let%test "adding and removing an element produces the empty set" =
  let v = mk Exp_app in
  let vs = Set.(empty |> add v |> remove v) in
  Set.is_empty vs

let%test "twice-adding and once-removing an element produces the empty set" =
  let v = mk Exp_app in
  let vs = Set.(empty |> add v |> add v |> remove v) in
  Set.is_empty vs
