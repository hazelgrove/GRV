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

module Wrap : sig
  type 'a t = { id : Id.t; value : 'a } [@@deriving sexp]

  val compare : 'a t -> 'a t -> int

  val mk : 'a -> 'a t

  val unmk : 'a t -> 'a

  val well_known : int -> 'a -> 'a t
end
