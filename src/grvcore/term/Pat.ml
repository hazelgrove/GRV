open OptionUtil.Syntax
open Sexplib0.Sexp_conv

module rec T : sig
  type t =
    | Var of Ingraph.t * string
    | Multiparent of Edge.t
    | Unicycle of Edge.t
    | Conflict of C.t
    | Hole of Vertex.t * GroveLang.position
  [@@deriving sexp]

  val compare : t -> t -> int
end = struct
  type t =
    | Var of Ingraph.t * string
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
  | Var (_, name) -> Some (PatVar name)
  | Multiparent _ | Unicycle _ | Conflict _ | Hole (_, _) -> None

let ingraph : t -> Ingraph.t option = function
  | Var (ingraph, _) -> Some ingraph
  | Multiparent edge | Unicycle edge ->
      Some (Ingraph.singleton edge EdgeState.Plus)
  | Conflict _ | Hole (_, _) -> None

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
  | Multiparent edge | Unicycle edge -> Graph.singleton edge EdgeState.Plus
  | Conflict children ->
      C.elements children |> List.map recomp |> List.map Graph.bindings
      |> List.concat |> Graph.of_list
  | Hole (_, _) -> Graph.empty

let construct (constructor : GroveLang.constructor) (pat : t) (u_gen : Id.Gen.t)
    : (GraphAction.t list * Id.Gen.t) option =
  match pat with
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
  | Var (_, _) | Multiparent _ | Unicycle _ -> (
      let* ingraph = ingraph pat in
      if not (GroveLang.sorts_equal constructor ingraph.invertex.constructor)
      then None
      else
        match GroveLang.default_position constructor with
        | Some position ->
            (* ConstructWrap *)
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
            (* ConstructConflict *)
            let sources, positions = Ingraph.sources ingraph |> List.split in
            let target, u_gen = Vertex.mk u_gen constructor in
            Some (GraphAction.construct_edges u_gen sources positions target))

let delete (pat : t) (u_gen : Id.Gen.t) : (GraphAction.t list * Id.Gen.t) option
    =
  match pat with
  | Conflict _ (* handled by apply_action *) | Hole (_, _) -> None
  | Var (_, _) | Multiparent _ | Unicycle _ ->
      (* Delete *)
      let+ ingraph = ingraph pat in
      let acc = Ingraph.delete ingraph in
      (acc, u_gen)

let relocate (source : Vertex.t) (position : GroveLang.position) (pat : t)
    (u_gen : Id.Gen.t) : (GraphAction.t list * Id.Gen.t) option =
  match pat with
  | Conflict _ (* handled by apply_action *) | Hole (_, _) -> None
  | Var (_, _) | Multiparent _ | Unicycle _ ->
      (* Reposition *)
      let* ingraph = ingraph pat in
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

let edit (edit_action : UserAction.edit) (pat : t) (u_gen : Id.Gen.t) :
    (GraphAction.t list * Id.Gen.t) option =
  match pat with
  | Conflict conflict ->
      (* Conflict *)
      let handler =
        match edit_action with
        | Construct constructor -> construct constructor
        | Delete -> delete
        | Relocate (vertex, position) -> relocate vertex position
      in
      C.construct_map handler conflict u_gen
  | Hole (_, _) | Var (_, _) | Multiparent _ | Unicycle _ -> (
      match edit_action with
      | Construct constructor -> construct constructor pat u_gen
      | Delete -> delete pat u_gen
      | Relocate (vertex, position) -> relocate vertex position pat u_gen)
