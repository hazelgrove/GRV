module OrderedType = struct
  type t = EdgeState.t * Edge.t [@@deriving sexp]

  let compare = compare
end

include OrderedType

let construct_edge (u_gen : Id.Gen.t) (source : Vertex.t)
    (position : GroveLang.Position.t) (target : Vertex.t) : t * Id.Gen.t =
  let edge, u_gen = Edge.mk u_gen source position target in
  ((EdgeState.Plus, edge), u_gen)

let construct_edges (u_gen : Id.Gen.t) (sources : Vertex.t list)
    (positions : GroveLang.Position.t list) (target : Vertex.t) :
    t list * Id.Gen.t =
  let acc, u_gen =
    List.fold_left2
      (fun (acc, u_gen) source position ->
        let ac, u_gen = construct_edge u_gen source position target in
        (ac :: acc, u_gen))
      ([], u_gen) sources positions
  in
  (List.rev acc, u_gen)

module Set = struct
  open Sexplib
  include Set.Make (OrderedType)

  let sexp_of_t (set : t) : Sexp.t =
    set |> elements |> Std.sexp_of_list OrderedType.sexp_of_t

  let t_of_sexp (sexp : Sexp.t) : t =
    sexp |> Std.list_of_sexp OrderedType.t_of_sexp |> of_list
end
