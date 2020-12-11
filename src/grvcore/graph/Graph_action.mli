(** Edge creation and deletion messages. *)

(** A message to set an edge to an edge state. *)
type t = { edge : Edge.t; state : Edge_state.t } [@@deriving sexp]

val to_string : t -> string

(** {1 Collections} *)

module Map : Map.S with type key = t

module Set : Set.S with type elt = t
