(** Edge creation and deletion messages. *)

type t = { edge : Edge.t; state : Edge_state.t } [@@deriving sexp]
(** A message to set an edge to an edge state. *)

(** {1 Collections} *)

module Map : Map.S with type key = t

module Set : Set.S with type elt = t

(** {1 String Conversions} *)

val to_string : t -> string
