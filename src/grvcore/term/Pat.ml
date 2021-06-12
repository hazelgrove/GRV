open OptionUtil.Syntax

module rec T : sig
  type t =
    | Var of Ingraph.t * string
    | Multiparent of Edge.t
    | Unicycle of Edge.t
    | Conflict of C.t
    | Hole

  val compare : t -> t -> int
end = struct
  type t =
    | Var of Ingraph.t * string
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
  | Var (_, name) -> Some (PatVar name)
  | Multiparent _ | Unicycle _ | Conflict _ | Hole -> None

let ingraph : t -> Ingraph.t option = function
  | Var (ingraph, _) -> Some ingraph
  | Multiparent edge | Unicycle edge -> Some (Ingraph.of_edge edge)
  | Conflict _ | Hole -> None

let rec decomp (graph : Graph.t) (edge : Edge.t) : t option =
  match edge.target.constructor with
  | PatVar x ->
      let+ ingraph = Ingraph.of_vertex edge.target graph in
      Var (ingraph, x)
  | Root | ExpVar _ | ExpLam | ExpApp | ExpNum _ | ExpPlus | ExpTimes | TypArrow
  | TypNum ->
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
  | Var (ingraph, _) -> ingraph.graph
  | Multiparent edge | Unicycle edge -> Graph.singleton edge EdgeState.Plus
  | Conflict children ->
      C.elements children |> List.map recomp |> List.map Graph.bindings
      |> List.concat |> Graph.of_list
  | Hole -> Graph.empty
