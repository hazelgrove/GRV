(** Environment to hold key value mappings *)
module Map = Map.Make (struct
  type t = string

  let compare = String.compare
end)

include Map
