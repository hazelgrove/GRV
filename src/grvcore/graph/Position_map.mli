(** Position maps *)

include Map.S with type key = Lang.Position.t
(** Maps [Lang.Position.t]s to child representations. *)

val get : ?default:'a list -> key -> 'a list t -> 'a list
val push : key -> 'a -> 'a list t -> 'a list t
