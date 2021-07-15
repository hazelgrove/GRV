type id = Id.t [@@deriving sexp]

type t = { id : id; constructor : GroveLang.Constructor.t } [@@deriving sexp]

let root : t =
  let id = Id.root in
  let constructor = GroveLang.Constructor.Root in
  { id; constructor }

let is_root (vertex : t) : bool = Id.is_root vertex.id

let mk (u_gen : Id.Gen.t) (constructor : GroveLang.Constructor.t) : t * Id.Gen.t
    =
  let id, u_gen = Id.Gen.next u_gen in
  ({ id; constructor }, u_gen)

module OrderedType = struct
  type nonrec t = t

  let compare (v1 : t) (v2 : t) : int = Id.compare v1.id v2.id
end

module Set = Set.Make (OrderedType)
module Map = Map.Make (OrderedType)
