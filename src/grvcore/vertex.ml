(** Graph vertices

    A [Vertex] encodes an {!Ast.HExp} constructor sort and type. *)

module Vertex = struct
  (** Indicates the sort and type of an [HExp] constructor. *)
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

  let mk (ctor : ctor) : t = Uuid.wrap ctor

  (** [Vertex]es are compared by comparing their {!field:Uuid.t.id}s *)
  let compare : t -> t -> int = Uuid.compare

  (** The root [Vertex] of all graphs. *)
  let root : t = { id = 0; value = Root_root }

  (** {1 Pretty Printing} *)

  let pp : Format.formatter -> t -> unit = Uuid.pp pp_ctor
end

include Vertex

module Map = Map.Make (Vertex)
(** An abstract map with [Vertex]es for keys *)
