(** Identifiable and comparable constructor instances. *)

type t = Lang.Constructor.t Uuid.wrap [@@deriving sexp]
(** Vertices carry a unique identifier that allow structurally equivalent
    instances to be distinguished and ordered. *)

val mk : Lang.Constructor.t -> t
(** [mk k] returns an instance of constructor [k]. *)

val root : t
(** The root vertex. *)

(** {1 Collections} *)

module Map : Map.S with type key = t

module Set : Set.S with type elt = t

(** {1 String Conversions} *)

val to_string : t -> string

val set_to_string : Set.t -> string
