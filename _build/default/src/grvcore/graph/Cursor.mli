(** A point of focus for graph actions. *)

type t = { vertex : Vertex.t; position : Lang.Position.t } [@@deriving sexp]
(** A cursor references a position relative to some vertex. *)

val to_string : t -> string

val root : t
(** The root cursor. *)
