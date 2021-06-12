type t = { id : Id.t; constructor : GroveLang.constructor }

module OrderedType = struct
  type nonrec t = t

  let compare (v1 : t) (v2 : t) : int = Id.compare v1.id v2.id
end

module Set = Set.Make (OrderedType)
module Map = Map.Make (OrderedType)
