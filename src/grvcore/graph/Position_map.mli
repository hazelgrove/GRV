(** Position maps *)

(** A position map maps [Lang.Position.t]s to child representations. *)
include Map.S with type key = Lang.Position.t

val get : ?default:'a list -> key -> 'a list t -> 'a list

val push : key -> 'a -> 'a list t -> 'a list t
