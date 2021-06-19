open OptionUtil.Syntax

module rec T : sig
  type t =
    | Var of Ingraph.t * string
    | Lam of Ingraph.t * Pat.t * Typ.t * t
    | App of Ingraph.t * t * t
    | Num of Ingraph.t * int
    | Plus of Ingraph.t * t * t
    | Times of Ingraph.t * t * t
    | Multiparent of Edge.t
    | Unicycle of Edge.t
    | Conflict of C.t
    | Hole of Vertex.t * GroveLang.position

  val compare : t -> t -> int
end = struct
  type t =
    | Var of Ingraph.t * string
    | Lam of Ingraph.t * Pat.t * Typ.t * t
    | App of Ingraph.t * t * t
    | Num of Ingraph.t * int
    | Plus of Ingraph.t * t * t
    | Times of Ingraph.t * t * t
    | Multiparent of Edge.t
    | Unicycle of Edge.t
    | Conflict of C.t
    | Hole of Vertex.t * GroveLang.position

  let compare = compare
end

and C : (Conflict.S with type elt = T.t) = Conflict.Make (T)

include T

let constructor : t -> GroveLang.constructor option = function
  | Var (_, name) -> Some (ExpVar name)
  | Lam (_, _, _, _) -> Some ExpLam
  | App (_, _, _) -> Some ExpApp
  | Num (_, n) -> Some (ExpNum n)
  | Plus (_, _, _) -> Some ExpPlus
  | Times (_, _, _) -> Some ExpTimes
  | Multiparent _ | Unicycle _ | Conflict _ | Hole (_, _) -> None

let ingraph : t -> Ingraph.t option = function
  | Var (ingraph, _)
  | Lam (ingraph, _, _, _)
  | App (ingraph, _, _)
  | Num (ingraph, _)
  | Plus (ingraph, _, _)
  | Times (ingraph, _, _) ->
      Some ingraph
  | Multiparent edge | Unicycle edge ->
      Some (Ingraph.singleton edge EdgeState.Plus)
  | Conflict _ | Hole (_, _) -> None

let rec decomp (graph : Graph.t) (edge : Edge.t) : t option =
  match edge.target.constructor with
  | ExpVar x ->
      let+ ingraph = Ingraph.of_vertex edge.target graph in
      Var (ingraph, x)
  | ExpLam ->
      let* ingraph = Ingraph.of_vertex edge.target graph in
      let* q = Pat.decomp' graph edge GroveLang.LamParam in
      let* t = Typ.decomp' graph edge GroveLang.LamType in
      let+ e = decomp' graph edge GroveLang.LamBody in
      Lam (ingraph, q, t, e)
  | ExpApp ->
      let* ingraph = Ingraph.of_vertex edge.target graph in
      let* e1 = decomp' graph edge GroveLang.AppFun in
      let+ e2 = decomp' graph edge GroveLang.AppArg in
      App (ingraph, e1, e2)
  | ExpNum n ->
      let+ ingraph = Ingraph.of_vertex edge.target graph in
      Num (ingraph, n)
  | ExpPlus ->
      let* ingraph = Ingraph.of_vertex edge.target graph in
      let* e1 = decomp' graph edge GroveLang.AppFun in
      let+ e2 = decomp' graph edge GroveLang.AppArg in
      Plus (ingraph, e1, e2)
  | ExpTimes ->
      let* ingraph = Ingraph.of_vertex edge.target graph in
      let* e1 = decomp' graph edge GroveLang.AppFun in
      let+ e2 = decomp' graph edge GroveLang.AppArg in
      Times (ingraph, e1, e2)
  | Root | PatVar _ | TypArrow | TypNum -> None

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
  | Var (ingraph, _) -> ingraph.graph
  | Lam (ingraph, pat, ty, body) ->
      let ingraph_bindings = Graph.bindings ingraph.graph in
      let pat_bindings = Graph.bindings (Pat.recomp pat) in
      let ty_bindings = Graph.bindings (Typ.recomp ty) in
      let body_bindings = Graph.bindings (recomp body) in
      Graph.of_list
        (ingraph_bindings @ pat_bindings @ ty_bindings @ body_bindings)
  | App (ingraph, e1, e2) ->
      let ingraph_bindings = Graph.bindings ingraph.graph in
      let e1_bindings = Graph.bindings (recomp e1) in
      let e2_bindings = Graph.bindings (recomp e2) in
      Graph.of_list (ingraph_bindings @ e1_bindings @ e2_bindings)
  | Num (ingraph, _) -> ingraph.graph
  | Plus (ingraph, e1, e2) | Times (ingraph, e1, e2) ->
      let ingraph_bindings = Graph.bindings ingraph.graph in
      let e1_bindings = Graph.bindings (recomp e1) in
      let e2_bindings = Graph.bindings (recomp e2) in
      Graph.of_list (ingraph_bindings @ e1_bindings @ e2_bindings)
  | Multiparent edge | Unicycle edge -> Graph.singleton edge EdgeState.Plus
  | Conflict children ->
      C.elements children |> List.map recomp |> List.map Graph.bindings
      |> List.concat |> Graph.of_list
  | Hole (_, _) -> Graph.empty

let construct (constructor : GroveLang.constructor) (exp : t) (u_gen : Id.Gen.t)
    : (GraphAction.t list * Id.Gen.t) option =
  match exp with
  | Conflict _ -> None
  | Hole (source, position) ->
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
  | Var (_, _)
  | Lam (_, _, _, _)
  | App (_, _, _)
  | Num (_, _)
  | Plus (_, _, _)
  | Times (_, _, _)
  | Multiparent _ | Unicycle _ -> (
      let* ingraph = ingraph exp in
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

let delete (exp : t) (u_gen : Id.Gen.t) : (GraphAction.t list * Id.Gen.t) option
    =
  match exp with
  | Conflict _ | Hole (_, _) -> None
  | Var (_, _)
  | Lam (_, _, _, _)
  | App (_, _, _)
  | Num (_, _)
  | Plus (_, _, _)
  | Times (_, _, _)
  | Multiparent _ | Unicycle _ ->
      let+ ingraph = ingraph exp in
      let acc = Ingraph.delete ingraph in
      (acc, u_gen)

let reposition (source : Vertex.t) (position : GroveLang.position) (exp : t)
    (u_gen : Id.Gen.t) : (GraphAction.t list * Id.Gen.t) option =
  match exp with
  | Conflict _ | Hole (_, _) -> None
  | Var (_, _)
  | Lam (_, _, _, _)
  | App (_, _, _)
  | Num (_, _)
  | Plus (_, _, _)
  | Times (_, _, _)
  | Multiparent _ | Unicycle _ ->
      let* ingraph = ingraph exp in
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

let apply_action (action : UserAction.t) (exp : t) (u_gen : Id.Gen.t) :
    (GraphAction.t list * Id.Gen.t) option =
  match exp with
  | Conflict c ->
      let handler =
        match action with
        | Construct constructor -> construct constructor
        | Delete -> delete
        | Reposition (vertex, position) -> reposition vertex position
      in
      C.construct_map handler c u_gen
  | Hole (_, _)
  | Var (_, _)
  | Lam (_, _, _, _)
  | App (_, _, _)
  | Num (_, _)
  | Plus (_, _, _)
  | Times (_, _, _)
  | Multiparent _ | Unicycle _ -> (
      match action with
      | Construct constructor -> construct constructor exp u_gen
      | Delete -> delete exp u_gen
      | Reposition (vertex, position) -> reposition vertex position exp u_gen)
