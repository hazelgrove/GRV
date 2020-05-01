open Format

type 'a t = { uuid : int; value : 'a }

let compare (u1 : 'a t) (u2 : 'a t) : int = Int.compare u1.uuid u2.uuid

let pp (pp' : formatter -> 'a -> unit) (fmt : formatter) (u : 'a t) : unit =
  fprintf fmt "%d=%a" u.uuid pp' u.value

let seed : int ref = ref 0

(* (\* TODO: let set_seed : int -> int -> unit = failwith __LOC__ *\)
 * let next_uuid () : int =
 *   seed := !seed + 1;
 *   !seed *)

let unwrap (u : 'a t) : 'a = u.value

let wrap (a : 'a) : 'a t =
  let next () =
    seed := !seed + 1;
    !seed
  in
  { uuid = next (); value = a }

let bind (u : 'a t) ~(f : int * 'a -> 'b t) : 'b t = f (u.uuid, u.value)

let return = wrap

module Map = Map.Make (Int)
