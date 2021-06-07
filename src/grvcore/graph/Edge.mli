(** Simple, directed, positioned edges between vertices. *)

type t' = { source : Cursor.t; target : Vertex.t } [@@deriving sexp]
(** An edge connects one vertex to another through an originating position. *)

type t = t' Uuid.wrap [@@deriving sexp]
(** Edges carry a unique identifier that allow structurally equivalent instances
    to be distinguished and ordered. *)

(** {1 Construction} *)

val mk : Cursor.t -> Vertex.t -> t
(** [mk c v] returns an instance of an edge from [c] to [v]. *)

(** {1 Collections} *)

module Map : Map.S with type key = t

module Set : Set.S with type elt = t

val partition_set : Set.t -> Vertex.t -> Set.t * Set.t
(** [partition_set s v] returns a pair of sets [(s1, s2)], where [s1] is the set
    of edges of [s] originating from [v], and [s2] is the set of edges that do
    not. *)

val union_sets : Set.t list -> Set.t
(** Unions a list of sets. *)

(** {1 String Conversions} *)

val to_string : t -> string

val set_to_string : Set.t -> string
