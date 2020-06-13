module Id : sig
  type t [@@deriving sexp_of]

  val compare : t -> t -> int

  val next : unit -> t

  (* Allows the allocation of `Id.t` that should be the same on all systems.  I.e., they are well known. *)
  val well_known : int -> t

  val show : t -> string

  val read : string -> t

  val pp : Format.formatter -> t -> unit
end = struct
  open Sexplib0.Sexp_conv

  type t = int [@@deriving sexp_of]

  let pp (fmt : Format.formatter) (id : t) : unit = Format.fprintf fmt "%d" id

  let compare = Int.compare

  let seed : t ref = ref 0

  let next () =
    seed := !seed + 1;
    !seed

  let well_known int =
    let () = assert (int <= 0) in
    int

  let show = Int.to_string

  let read = int_of_string
end

module Map = Map.Make (Id)
module Set = Set.Make (Id)

(* TODO: use `private` types for Wrap.t *)
module Wrap = struct
  type 'a t = { id : Id.t; value : 'a } [@@deriving sexp_of]

  let compare (u1 : 'a t) (u2 : 'a t) : int = Id.compare u1.id u2.id

  let mk (a : 'a) : 'a t = { id = Id.next (); value = a }

  let unmk (u : 'a t) : 'a = u.value

  let well_known (int : int) (a : 'a) : 'a t =
    { id = Id.well_known int; value = a }

  let bind (u : 'a t) (f : Id.t * 'a -> 'b t) : 'b t = f (u.id, u.value)

  let return = mk

  let pp (pp' : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
      (u : 'a t) : unit =
    Format.fprintf fmt "%s=%a" (Id.show u.id) pp' u.value
end

(* TODO: *)
module Embedded = struct
  (* TODO: module defining how to extract an ID *)
  (* module GetId = struct
       type nonrec t = t
       let id (i : t) = i.id
     end
     module OrderedType = Uuid.OrderedType(GetId) *)
end
