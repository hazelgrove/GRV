type t = Exp of Exp.t | Pat of Pat.t | Typ of Typ.t

module Set = Set.Make (struct
  type nonrec t = t

  let compare = compare
end)
