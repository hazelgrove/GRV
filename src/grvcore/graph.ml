module Vertex = struct
  type t = ctor Uuid.t

  and ctor =
    | Root_root
    | Id_id of string
    | Exp_lam
    | Exp_app
    | Exp_var
    | Typ_app
    | Typ_var

  let compare : t -> t -> int = Uuid.compare
end

(* Edge *)

type index =
  | Root_root_root
  | Exp_lam_param
  | Exp_lam_param_type
  | Exp_lam_body
  | Exp_app_fun
  | Exp_app_arg
  | Exp_var_id
  | Typ_app_fun
[@@deriving compare]

module Edge = struct
  type t = t' Uuid.t

  and t' = { source : Vertex.t; index : index; target : Vertex.t }

  type state = Created | Destroyed

  let compare : t -> t -> int = Uuid.compare

  let equal (edge1 : t) (edge2 : t) : bool = compare edge1 edge2 = 0

  let source (edge : t) : Vertex.t = (Uuid.unwrap edge).source

  let target (edge : t) : Vertex.t = (Uuid.unwrap edge).target

  let index (edge : t) : index = (Uuid.unwrap edge).index
end

module UuidMap = Map.Make (Uuid.OrderedType)
module EdgeMap = Map.Make (Edge)

module EdgeSet = Set.Make (struct
  type t = Edge.t

  let compare : t -> t -> int = Edge.compare
end)

(* Vertex *)

module VertexMap = struct
  include Map.Make (Vertex)

  let obtain (key : Vertex.t) (map : EdgeSet.t t) : EdgeSet.t =
    Option.value (find_opt key map) ~default:EdgeSet.empty
end

module VertexIndex = struct
  type t = Vertex.t * index

  let compare ((v1, i1) : t) ((v2, i2) : t) : int =
    match Uuid.compare v1 v2 with 0 -> compare_index i1 i2 | x -> x
end

module VertexIndexMap = struct
  include Map.Make (VertexIndex)

  let obtain (key : VertexIndex.t) (map : EdgeSet.t t) : EdgeSet.t =
    Option.value (find_opt key map) ~default:EdgeSet.empty
end

(* Graph *)

type t = {
  (* Maps Edge id to edge *)
  edges : Edge.t UuidMap.t;
  (* Note: edges not in the table have not been created yet and are `\bot` *)
  edge_states : Edge.state EdgeMap.t;
  (* Maps Vertex id to parent edge *)
  edges_to : EdgeSet.t VertexMap.t;
  (* Maps Vertex id and Child index to set of edges *)
  edges_from : EdgeSet.t VertexIndexMap.t;
}

let empty : t =
  {
    edges : Edge.t UuidMap.t = UuidMap.empty;
    edge_states : Edge.state EdgeMap.t = EdgeMap.empty;
    edges_to : EdgeSet.t VertexMap.t = VertexMap.empty;
    edges_from : EdgeSet.t VertexIndexMap.t = VertexIndexMap.empty;
  }

let add_edge (graph : t) (edge : Edge.t) : t =
  let target = Edge.target edge in
  let source = (Edge.source edge, Edge.index edge) in
  {
    graph with
    (* add to target's parents *)
    edges_to : EdgeSet.t VertexMap.t =
      (let parents = VertexMap.obtain target graph.edges_to in
       VertexMap.add target (EdgeSet.add edge parents) graph.edges_to);
    (* add to source's children *)
    edges_from : EdgeSet.t VertexIndexMap.t =
      (let children = VertexIndexMap.obtain source graph.edges_from in
       VertexIndexMap.add source (EdgeSet.add edge children) graph.edges_from);
  }

let drop_edge (graph : t) (edge : Edge.t) : t =
  let target = Edge.target edge in
  let source = (Edge.source edge, Edge.index edge) in
  {
    graph with
    (* drop from target's parents *)
    edges_to : EdgeSet.t VertexMap.t =
      (let parents = VertexMap.obtain target graph.edges_to in
       VertexMap.add target
         (EdgeSet.filter (Edge.equal edge) parents)
         graph.edges_to);
    (* drop from source's children *)
    edges_from : EdgeSet.t VertexIndexMap.t =
      (let children = VertexIndexMap.obtain source graph.edges_from in
       VertexIndexMap.add source
         (EdgeSet.filter (Edge.equal edge) children)
         graph.edges_from);
  }

let update_edge (graph : t) (edge : Edge.t) (edge_state : Edge.state) : t =
  let old_state = EdgeMap.find_opt edge graph.edge_states in
  let action : Edge.state option =
    match old_state with
    | Some Destroyed -> None
    | Some Created -> (
        match edge_state with Destroyed -> Some Destroyed | Created -> None )
    | None -> Some edge_state
  in
  match action with
  | None -> graph
  | Some Created ->
      (* TODO: assert not already exists? *)

      (* TODO: short circuit if deleting a non-existant *)
      add_edge graph edge
  | Some Destroyed -> (
      let edge_states : Edge.state EdgeMap.t =
        EdgeMap.add edge Edge.Destroyed graph.edge_states
      in
      match old_state with
      | None -> { graph with edge_states }
      | _ -> drop_edge { graph with edge_states } edge )
