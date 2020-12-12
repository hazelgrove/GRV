(** Unique, comparable identifiers. *)

module Id : sig
  type t [@@deriving sexp]

  val compare : t -> t -> int

  val next : unit -> t

  (* Allows the allocation of `Id.t` that should be the same on all systems.  I.e., they are well known. *)
  val well_known : int -> t

  val to_string : t -> string

  val of_string : string -> t
end

module Map : Map.S with type key = Id.t

module Set : Set.S with type elt = Id.t

type 'a wrap = { id : Id.t; value : 'a } [@@deriving sexp]

val compare : 'a wrap -> 'a wrap -> int

val wrap : 'a -> 'a wrap

val unwrap : 'a wrap -> 'a

val well_known : int -> 'a -> 'a wrap
