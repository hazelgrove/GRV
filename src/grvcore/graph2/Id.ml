include Sexplib.Std

module OrderedType = struct
  type t = int [@@deriving sexp]

  let compare : t -> t -> int = compare
end

include OrderedType

let root : t = 0

module Gen = struct
  type id = t

  type t = int [@@deriving sexp]

  let init : t = 1

  let next (u_gen : t) : id * t = (u_gen, u_gen + 1)
end

module Map = struct
  open Sexplib
  include Map.Make (OrderedType)

  let sexp_of_t (sexp_of_value : 'a -> Sexp.t) (map : 'a t) : Sexp.t =
    Util.Sexp.of_map (bindings map) OrderedType.sexp_of_t sexp_of_value

  let t_of_sexp (value_of_sexp : Sexp.t -> 'a) (sexp : Sexp.t) : 'a t =
    sexp
    |> Std.list_of_sexp (function
         | List [ key_sexp; value_sexp ] ->
             (OrderedType.t_of_sexp key_sexp, value_of_sexp value_sexp)
         | _ -> failwith __LOC__)
    |> List.to_seq |> of_seq
end
