type t = int

let compare : t -> t -> int = compare

module Gen = struct
  type id = t

  type t = int

  let init : t = 1

  let next (u_gen : t) : id * t = (u_gen, u_gen + 1)
end
