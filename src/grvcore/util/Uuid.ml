module Id = struct
  open Sexplib0.Sexp_conv

  type t = int [@@deriving sexp]

  let compare = Int.compare

  let seed : t ref = ref 0

  let next () =
    seed := !seed + 1;
    !seed

  let well_known int =
    let () = assert (int <= 0) in
    int

  let to_string = Int.to_string

  let of_string = int_of_string
end

module Map = Map.Make (Id)
module Set = Set.Make (Id)

(* TODO: use `private` types for wrap *)
type 'a wrap = { id : Id.t; value : 'a } [@@deriving sexp]

let compare (u1 : 'a wrap) (u2 : 'a wrap) : int = Id.compare u1.id u2.id

let wrap (a : 'a) : 'a wrap = { id = Id.next (); value = a }

let unwrap (u : 'a wrap) : 'a = u.value

let well_known (int : int) (a : 'a) : 'a wrap =
  { id = Id.well_known int; value = a }

(* module Embedded = struct
  (* TODO: module defining how to extract an ID *)
  (* module GetId = struct
       type nonrec t = t
       let id (i : t) = i.id
     end
     module OrderedType = Uuid.OrderedType(GetId) *)
end *)
