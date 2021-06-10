type t = { id : Id.t; constructor : GroveLang.constructor }

module OrderedType = struct
  type nonrec t = t

  let compare = compare
end

module Set = Set.Make (OrderedType)
