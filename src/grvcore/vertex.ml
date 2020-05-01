(** A vertex in the program graph *)

module Vertex = struct
  type ctor =
    | Root_root
    | Id_id of string
    | Exp_lam
    | Exp_app
    | Exp_var
    | Typ_app
    | Typ_var
  [@@deriving show]

  type t = ctor Uuid.t

  let root : t = { uuid = 0; value = Root_root }

  let mk (ctor : ctor) : t = Uuid.wrap ctor

  let compare : t -> t -> int = Uuid.compare

  let pp : Format.formatter -> t -> unit = Uuid.pp pp_ctor
end

include Vertex
module Map = Map.Make (Vertex)
