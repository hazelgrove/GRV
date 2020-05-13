type t = Constructor.t Uuid.Wrap.t

let mk : Constructor.t -> t = Uuid.Wrap.mk

let compare : t -> t -> int = Uuid.Wrap.compare

let root : t = Uuid.Wrap.well_known 0 Constructor.Root_root

let pp : Format.formatter -> t -> unit = Uuid.Wrap.pp Constructor.pp

module OrderedType = struct
  type nonrec t = t

  let compare = compare
end

module Map = Map.Make (OrderedType)
