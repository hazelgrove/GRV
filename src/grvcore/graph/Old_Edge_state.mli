(** Edge states *)

(** An extant edge is either created or deleted. *)
type t = Created | Deleted [@@deriving sexp]

(** {1 String Conversions} *)

val to_string : t -> string
