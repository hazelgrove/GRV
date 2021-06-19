type t = EdgeState.t * Edge.t

let construct_edge (u_gen : Id.Gen.t) (source : Vertex.t)
    (position : GroveLang.position) (target : Vertex.t) : t * Id.Gen.t =
  let edge, u_gen = Edge.mk u_gen source position target in
  ((EdgeState.Plus, edge), u_gen)

let construct_edges (u_gen : Id.Gen.t) (sources : Vertex.t list)
    (positions : GroveLang.position list) (target : Vertex.t) :
    t list * Id.Gen.t =
  let acc, u_gen =
    List.fold_left2
      (fun (acc, u_gen) source position ->
        let ac, u_gen = construct_edge u_gen source position target in
        (ac :: acc, u_gen))
      ([], u_gen) sources positions
  in
  (List.rev acc, u_gen)
