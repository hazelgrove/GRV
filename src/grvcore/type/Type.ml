(* Note the following meanings for Hole:
   - Hole [] = Unknown type
   - Hole [t1; t2] = Type conflict
   - Hole [t] = Kind inconsistency
*)

(* Hole (int; bool) vs String is an error *)
(* Could factor working parts of types in a hole *)
(* Could treat use of Hole as union types *)

type t = Unknown | Num | Arrow of t * t [@@deriving show]
