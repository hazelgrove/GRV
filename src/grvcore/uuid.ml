(** Universally unique identifiers *)

open Format

(** A type that can be ordered and uniquely identified. *)
module Id = struct
  type t = int

  let compare = Int.compare
end

type 'a t = { uuid : Id.t; value : 'a }
(** A value tagged with an [Id]. *)

(** [Uuid]s are compared by comparing their [uuid]s. *)
let compare (u1 : 'a t) (u2 : 'a t) : int = Id.compare u1.uuid u2.uuid

(**/**)

let seed : int ref = ref 0

(* TODO: let set_seed : int -> int -> unit = failwith __LOC__ *)

(**/**)

(** Tags a value with a fresh [id]. *)
let wrap (a : 'a) : 'a t =
  let next () =
    seed := !seed + 1;
    !seed
  in
  { uuid = next (); value = a }

(** Strips the [id] from a tagged value. *)
let unwrap (u : 'a t) : 'a = u.value

(** {1 Monadic API} *)

(** Transforms the fields of one [Uuid] into another. *)
let bind (u : 'a t) ~(f : int * 'a -> 'b t) : 'b t = f (u.uuid, u.value)

let return = wrap

(** {1 Pretty Printing} *)

let pp (pp' : formatter -> 'a -> unit) (fmt : formatter) (u : 'a t) : unit =
  fprintf fmt "%d=%a" u.uuid pp' u.value

(** {1 Collections } *)

module Map = Map.Make (Id)
(** A map with [Id] keys *)
