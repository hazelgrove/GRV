(** Action focus points. *)

(** A cursor references a position relative to some vertex. *)
type t = { vertex : Vertex.t; position : Lang.Position.t } [@@deriving sexp]

val to_string : t -> string

val root : t
(** The root cursor. *)

(** {1 Collections} *)

module Map : Map.S with type key = t

module Set : Set.S with type elt = t
