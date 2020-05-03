(** Graph vertices

    A [Vertex] encodes an {!Ast.HExp} constructor sort and type. *)

module Vertex = struct
  (** Indicates the sort and type of an [HExp] constructor. *)

  type t = Constructor.t Uuid.t

  let mk (ctor : Constructor.t) : t = Uuid.wrap ctor

  (** [Vertex]es are compared by comparing their {!field:Uuid.t.id}s *)
  let compare : t -> t -> int = Uuid.compare

  (** The root [Vertex] of all graphs. *)
  let root : t = { id = 0; value = Root_root }

  (** {1 Pretty Printing} *)

  let pp : Format.formatter -> t -> unit = Uuid.pp Constructor.pp
end

include Vertex

(** An abstract map with [Vertex]es for keys *)
module Map = Map.Make (Vertex)
