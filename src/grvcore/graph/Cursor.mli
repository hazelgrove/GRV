(** A point of focus for graph actions. *)

(** A cursor references a position relative to some vertex. *)
type t = { vertex : Vertex.t; position : Lang.Position.t } [@@deriving sexp]

val to_string : t -> string

val root : t
(** The root cursor. *)
