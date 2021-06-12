open OptionUtil.Syntax

module rec T : sig
  type t =
    | Arrow of Ingraph.t * t * t
    | Num of Ingraph.t
    | Multiparent of Edge.t
    | Unicycle of Edge.t
    | Conflict of C.t
    | Hole

  val compare : t -> t -> int
end = struct
  type t =
    | Arrow of Ingraph.t * t * t
    | Num of Ingraph.t
    | Multiparent of Edge.t
    | Unicycle of Edge.t
    | Conflict of C.t
    | Hole

  let compare = compare
end

and C : sig
  type t

  val of_list : T.t list -> t

  val elements : t -> T.t list
end =
  Set.Make (T)

include T

let constructor : t -> GroveLang.constructor option = function
  | Arrow (_, _, _) -> Some TypArrow
  | Num _ -> Some TypNum
  | Multiparent _ | Unicycle _ | Conflict _ | Hole -> None

let ingraph : t -> Ingraph.t option = function
  | Arrow (ingraph, _, _) | Num ingraph -> Some ingraph
  | Multiparent edge | Unicycle edge -> Some (Ingraph.of_edge edge)
  | Conflict _ | Hole -> None

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
  | Hole -> Graph.empty
