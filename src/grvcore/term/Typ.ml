open OptionUtil.Syntax

module rec T : sig
  type t =
    | Arrow of Ingraph.t * t * t
    | Num of Ingraph.t
    | Multiparent of Edge.t
    | Unicycle of Edge.t
    | Conflict of C.t
    | Hole of Vertex.t * GroveLang.position
  [@@deriving sexp]

  val compare : t -> t -> int
end = struct
  type t =
    | Arrow of Ingraph.t * t * t
    | Num of Ingraph.t
    | Multiparent of Edge.t
    | Unicycle of Edge.t
    | Conflict of C.t
    | Hole of Vertex.t * GroveLang.position
  [@@deriving sexp]

  let compare = compare
end

and C : (Conflict.S with type elt = T.t) = Conflict.Make (T)

include T

let constructor : t -> GroveLang.constructor option = function
  | Arrow (_, _, _) -> Some TypArrow
  | Num _ -> Some TypNum
  | Multiparent _ | Unicycle _ | Conflict _ | Hole (_, _) -> None

let ingraph : t -> Ingraph.t option = function
  | Arrow (ingraph, _, _) | Num ingraph -> Some ingraph
  | Multiparent edge | Unicycle edge ->
      Some (Ingraph.singleton edge EdgeState.Plus)
  | Conflict _ | Hole (_, _) -> None

let rec decomp (graph : Graph.t) (edge : Edge.t) : t option =
  match edge.target.constructor with
  | TypArrow ->
      let* ingraph = Ingraph.of_vertex edge.target graph in
      let* ty1 = decomp' graph edge GroveLang.ArrowArg in
      let+ ty2 = decomp' graph edge GroveLang.ArrowResult in
      Arrow (ingraph, ty1, ty2)
  | TypNum ->
      let+ ingraph = Ingraph.of_vertex edge.target graph in
      Num ingraph
  | Root | ExpVar _ | ExpLam | ExpApp | ExpNum _ | ExpPlus | ExpTimes | PatVar _
    ->
      None

and decomp' (graph : Graph.t) (edge : Edge.t) (position : GroveLang.position) :
    t option =
  let outedges = Graph.outedges edge.target position graph in
  match Edge.Set.cardinal outedges with
  | 0 -> Some (Hole (edge.target, position))
  | 1 -> (
      let* outedge = Edge.Set.choose_opt outedges in
      match Vertex.Set.cardinal (Graph.parents outedge.target graph) with
      | 0 -> None
      | _ ->
          let ancestors = Graph.ancestors outedge.target graph in
          let* unicycle_root = Vertex.Set.min_elt_opt ancestors in
          if outedge.target = unicycle_root then Some (Unicycle edge)
          else decomp graph outedge)
  | _ ->
      let+ children =
        Edge.Set.elements outedges
        |> List.map (decomp graph)
        |> OptionUtil.of_list
      in
      Conflict (C.of_list children)

let rec recomp : t -> Graph.t = function
  | Arrow (ingraph, ty1, ty2) ->
      let ingraph_bindings = Graph.bindings ingraph.graph in
      let ty1_bindings = Graph.bindings (recomp ty1) in
      let ty2_bindings = Graph.bindings (recomp ty2) in
      Graph.of_list (ingraph_bindings @ ty1_bindings @ ty2_bindings)
  | Num ingraph -> ingraph.graph
  | Multiparent edge | Unicycle edge -> Graph.singleton edge EdgeState.Plus
  | Conflict children ->
      C.elements children |> List.map recomp |> List.map Graph.bindings
      |> List.concat |> Graph.of_list
  | Hole (_, _) -> Graph.empty

let construct (constructor : GroveLang.constructor) (typ : t) (u_gen : Id.Gen.t)
    : (GraphAction.t list * Id.Gen.t) option =
  match typ with
  | Conflict _ -> None (* handled by apply_action *)
  | Hole (source, position) ->
      (* Construct *)
      if
        not
          (GroveLang.is_valid_position position source.constructor constructor)
      then None
      else
        let target, u_gen = Vertex.mk u_gen constructor in
        let ac, u_gen =
          GraphAction.construct_edge u_gen source position target
        in
        Some ([ ac ], u_gen)
  | Num _ | Arrow (_, _, _) | Multiparent _ | Unicycle _ -> (
      let* ingraph = ingraph typ in
      if not (GroveLang.sorts_equal constructor ingraph.invertex.constructor)
      then None
      else
        match GroveLang.default_position constructor with
        | Some position ->
            if
              not
                (GroveLang.is_valid_position position
                   ingraph.invertex.constructor constructor)
            then None
            else
              let acc1 = Ingraph.delete ingraph in
              let sources, positions = Ingraph.sources ingraph |> List.split in
              let target, u_gen = Vertex.mk u_gen constructor in
              let acc2, u_gen =
                GraphAction.construct_edges u_gen sources positions target
              in
              let ac, u_gen =
                GraphAction.construct_edge u_gen target position
                  ingraph.invertex
              in
              Some (acc1 @ acc2 @ [ ac ], u_gen)
        | None ->
            let sources, positions = Ingraph.sources ingraph |> List.split in
            let target, u_gen = Vertex.mk u_gen constructor in
            Some (GraphAction.construct_edges u_gen sources positions target))

let delete (typ : t) (u_gen : Id.Gen.t) : (GraphAction.t list * Id.Gen.t) option
    =
  match typ with
  | Conflict _ | Hole (_, _) -> None
  | Num _ | Arrow (_, _, _) | Multiparent _ | Unicycle _ ->
      let+ ingraph = ingraph typ in
      let acc = Ingraph.delete ingraph in
      (acc, u_gen)

let relocate (source : Vertex.t) (position : GroveLang.position) (typ : t)
    (u_gen : Id.Gen.t) : (GraphAction.t list * Id.Gen.t) option =
  match typ with
  | Conflict _ | Hole (_, _) -> None
  | Num _ | Arrow (_, _, _) | Multiparent _ | Unicycle _ ->
      let* ingraph = ingraph typ in
      if
        not
          (GroveLang.is_valid_position position ingraph.invertex.constructor
             source.constructor)
      then None
      else
        let acc = Ingraph.delete ingraph in
        let ac, u_gen =
          GraphAction.construct_edge u_gen source position ingraph.invertex
        in
        Some (acc @ [ ac ], u_gen)

let edit (edit_action : UserAction.edit) (typ : t) (u_gen : Id.Gen.t) :
    (GraphAction.t list * Id.Gen.t) option =
  match typ with
  | Conflict c ->
      let handler =
        match edit_action with
        | Construct constructor -> construct constructor
        | Delete -> delete
        | Relocate (vertex, position) -> relocate vertex position
      in
      C.construct_map handler c u_gen
  | Hole (_, _) | Num _ | Arrow (_, _, _) | Multiparent _ | Unicycle _ -> (
      match edit_action with
      | Construct constructor -> construct constructor typ u_gen
      | Delete -> delete typ u_gen
      | Relocate (vertex, position) -> relocate vertex position typ u_gen)
