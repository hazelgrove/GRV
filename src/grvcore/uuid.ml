(** Universally unique identifiers *)

(* TODO narrow open scope *)
open Format

(** A type that can be ordered and uniquely identified. *)
module Id = struct
  type t = int

  let compare : t -> t -> int = Int.compare

  (**/**)

  (* TODO: let set_seed : int -> int -> unit = failwith __LOC__ *)

  let seed : t ref = ref 0

  (**/**)

  let next () =
    seed := !seed + 1;
    !seed
end

(** A value tagged with an [Id]. *)
type 'a t = { id : Id.t; value : 'a }

(** [Uuid]s are compared by comparing their [id]s. *)
let compare (u1 : 'a t) (u2 : 'a t) : int = Id.compare u1.id u2.id

(** Tags a value with a fresh [id]. *)
let wrap (a : 'a) : 'a t = { id = Id.next (); value = a }

(** Strips the [id] from a tagged value. *)
let unwrap (u : 'a t) : 'a = u.value

(** {1 Monadic API} *)

(** Transforms the fields of one [Uuid] into another. *)
let bind (u : 'a t) (f : int * 'a -> 'b t) : 'b t = f (u.id, u.value)

let return = wrap

(** {1 Pretty Printing} *)

let pp (pp' : formatter -> 'a -> unit) (fmt : formatter) (u : 'a t) : unit =
  fprintf fmt "%d=%a" u.id pp' u.value

(** {1 Collections } *)

(** An abstract map with [Id]s for keys. *)
module Map = Map.Make (Id)
