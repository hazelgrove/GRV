module T = struct
  type id = Id.t [@@deriving sexp]

  type t = {
    id : id;
    source : Vertex.t;
    position : GroveLang.Position.t;
    target : Vertex.t;
  }
  [@@deriving sexp]
end

include T

let mk (u_gen : Id.Gen.t) (source : Vertex.t) (position : GroveLang.Position.t)
    (target : Vertex.t) : t * Id.Gen.t =
  let id, u_gen = Id.Gen.next u_gen in
  ({ id; source; position; target }, u_gen)

module OrderedType = struct
  type nonrec t = t

  let compare = compare
end

module Set = struct
  include Set.Make (OrderedType)

  let targets (edges : t) : Vertex.Set.t =
    elements edges |> List.map (fun edge -> edge.target) |> Vertex.Set.of_list
end

module Map = struct
  open Sexplib
  include Map.Make (OrderedType)

  let sexp_of_t (sexp_of_value : 'a -> Sexp.t) (map : 'a t) : Sexp.t =
    Util.Sexp.of_map (bindings map) T.sexp_of_t sexp_of_value

  let t_of_sexp (value_of_sexp : Sexp.t -> 'a) (sexp : Sexp.t) : 'a t =
    sexp
    |> Std.list_of_sexp (function
         | List [ key_sexp; value_sexp ] ->
             (T.t_of_sexp key_sexp, value_of_sexp value_sexp)
         | _ -> failwith __LOC__)
    |> List.to_seq |> of_seq

  let keys (map : EdgeState.t t) : Set.t =
    bindings map |> List.map fst |> Set.of_list

  let of_list (bindings : (key * 'a) list) : 'a t =
    of_seq (List.to_seq bindings)
end
