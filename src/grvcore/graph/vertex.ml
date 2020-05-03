(** Graph vertices

    A [Vertex] encodes an {!Ast.HExp} constructor sort and type. *)

(** Indicates the sort and type of an [HExp] constructor. *)

type t = Constructor.t Uuid.Wrap.t

let mk : Constructor.t -> t = Uuid.Wrap.mk

(** [Vertex]es are compared by comparing their {!field:Uuid.t.id}s *)
let compare : t -> t -> int = Uuid.Wrap.compare

(** The root [Vertex] of all graphs. *)
let root : t = Uuid.Wrap.well_known 0 Constructor.Root_root

(** {1 Pretty Printing} *)

let pp : Format.formatter -> t -> unit = Uuid.Wrap.pp Constructor.pp

module OrderedType = struct
  type nonrec t = t

  let compare = compare
end

(** An abstract map with [Vertex]es for keys *)
module Map = Map.Make (OrderedType)
