type t = { id : Id.t; constructor : GroveLang.constructor }

let mk (u_gen : Id.Gen.t) (constructor : GroveLang.constructor) : t * Id.Gen.t =
  let id, u_gen = Id.Gen.next u_gen in
  ({ id; constructor }, u_gen)

module OrderedType = struct
  type nonrec t = t

  let compare (v1 : t) (v2 : t) : int = Id.compare v1.id v2.id
end

module Set = Set.Make (OrderedType)
module Map = Map.Make (OrderedType)
