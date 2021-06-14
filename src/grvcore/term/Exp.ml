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
    | Hole

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
    | Hole

  let compare = compare
end

and C : sig
  type t

  type elt = T.t

  val add : elt -> t -> t

  val of_list : T.t list -> t

  val elements : t -> T.t list
end =
  Set.Make (T)

include T

let constructor : t -> GroveLang.constructor option = function
  | Var (_, name) -> Some (ExpVar name)
  | Lam (_, _, _, _) -> Some ExpLam
  | App (_, _, _) -> Some ExpApp
  | Num (_, n) -> Some (ExpNum n)
  | Plus (_, _, _) -> Some ExpPlus
  | Times (_, _, _) -> Some ExpTimes
  | Multiparent _ | Unicycle _ | Conflict _ | Hole -> None

let ingraph : t -> Ingraph.t option = function
  | Var (ingraph, _)
  | Lam (ingraph, _, _, _)
  | App (ingraph, _, _)
  | Num (ingraph, _)
  | Plus (ingraph, _, _)
  | Times (ingraph, _, _) ->
      Some ingraph
  | Multiparent edge | Unicycle edge -> Some (Ingraph.of_edge edge)
  | Conflict _ | Hole -> None

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
  | 0 -> Some Hole
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
  | Hole -> Graph.empty