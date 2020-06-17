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
