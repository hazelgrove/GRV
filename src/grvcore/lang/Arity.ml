include Set.Make (struct
  type t = Position.t

  let compare = compare
end)
