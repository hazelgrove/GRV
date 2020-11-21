type t = Lang.Constructor.t Uuid.Wrap.t [@@deriving show, sexp]

let mk : Lang.Constructor.t -> t = Uuid.Wrap.mk

let pp : Format.formatter -> t -> unit = Uuid.Wrap.pp Lang.Constructor.pp

(* TODO: let equal : t -> t -> bool = Uuid.Wrap.equal *)

let compare : t -> t -> int = Uuid.Wrap.compare

let root : t = Uuid.Wrap.well_known 0 Lang.Constructor.Root_root

module OrderedType = struct
  type nonrec t = t

  let compare = compare
end

module Map = Map.Make (OrderedType)
module Set = Set.Make (OrderedType)

let print_set (vertexes : Set.t) : unit =
  Set.elements vertexes
  |> List.map (fun (v : t) -> Uuid.Id.show v.id)
  |> String.concat "; " |> Format.printf "[%s]%!"

(* Some tests of our assumptions about the semantics of (vertex) sets *)

let%test "empty set has cardinality 0" = Set.cardinal Set.empty = 0

let%test "empty set has no elements" = Set.elements Set.empty = []

let%test "singleton set has cardinality 1" =
  Set.cardinal (Set.add (mk Lang.Constructor.Exp_app) Set.empty) = 1

let%test "singleton set has one element" =
  let v = mk Lang.Constructor.Exp_app in
  Set.elements (Set.add v Set.empty) = [ v ]

let%test "adding the same element twice produces a singleton set" =
  let v = mk Lang.Constructor.Exp_app in
  let vs = Set.add v Set.empty in
  let vs' = Set.add v vs in
  Set.cardinal vs' = 1 && Set.elements vs' = [ v ]

let%test "adding the same element twice produces a singleton set" =
  let v = mk Lang.Constructor.Exp_app in
  let vs = Set.add v Set.empty in
  let vs' = Set.add v vs in
  Set.cardinal vs' = 1 && Set.elements vs' = [ v ]

let%test "adding the same element thrice produces a singleton set" =
  let v = mk Lang.Constructor.Exp_app in
  let vs = Set.add v Set.empty in
  let vs' = Set.add v vs in
  let vs'' = Set.add v vs' in
  Set.cardinal vs'' = 1 && Set.elements vs'' = [ v ]

let%test "adding and removing an element produces the empty set" =
  let v = mk Lang.Constructor.Exp_app in
  let vs = Set.add v Set.empty in
  let vs' = Set.remove v vs in
  Set.is_empty vs'

let%test "twice-adding and once-removing an element produces the empty set" =
  let v = mk Lang.Constructor.Exp_app in
  let vs = Set.add v Set.empty in
  let vs' = Set.add v vs in
  let vs'' = Set.remove v vs' in
  Set.is_empty vs''
