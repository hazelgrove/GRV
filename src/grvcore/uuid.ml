type t = int

let compare : t -> t -> int = Int.compare

let seed : int ref = ref 0

(* TODO: let set_seed : int -> int -> unit = failwith __LOC__ *)
let mk () : t =
  seed := !seed + 1;
  !seed

(* TODO: rename to "wrapped"?  *)
type 'a with_uuid = { uuid : t; value : 'a }

let wrap (value : 'a) : 'a with_uuid = { uuid = mk (); value }

let unwrap (wrapped_value : 'a with_uuid) : 'a = wrapped_value.value
