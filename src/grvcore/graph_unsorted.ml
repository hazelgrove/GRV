let compare_string = String.compare

type ctor =
  | Root_root
  | Id_id of string
  | Exp_lam
  | Exp_app
  | Exp_var
  | Typ_app
  | Typ_var
[@@deriving compare]

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

module Vertex = struct
  type t = ctor Uuid.t

  let compare : t -> t -> int = Uuid.compare
end

module Edge = struct
  type t = t' Uuid.t

  and t' = { source : Vertex.t; index : index; target : Vertex.t }

  let compare : t -> t -> int = Uuid.compare
end

type edge_state = Created | Destroyed [@@deriving compare]

(* module UuidMap = Map.Make (Uuid.OrderedType) *)
module EdgeMap = Map.Make (Edge)
module VertexMap = Map.Make (Vertex)

module VertexIndexMap = Map.Make (struct
  type t = Vertex.t * index

  let compare ((v1, i1) : t) ((v2, i2) : t) : int =
    match Uuid.compare v1 v2 with 0 -> compare_index i1 i2 | cmp -> cmp
end)

module EdgeSet = Set.Make (struct
  type t = Edge.t

  let compare : t -> t -> int = Edge.compare
end)

type graph = {
  (* Maps Edge id to edge *)
  (* edges : Edge.t UuidMap.t; *)
  (* Note: edges not in the table have not been created yet and are `\bot` *)
  edge_state : edge_state EdgeMap.t;
  (* Maps Vertex id to parent edge *)
  edges_to : EdgeSet.t VertexMap.t;
  (* Maps Vertex id and Child index to set of edges *)
  edges_from : EdgeSet.t VertexIndexMap.t;
}

let init : graph =
  {
    (* edges = UuidMap.empty; *)
    edge_state = EdgeMap.empty;
    edges_to = VertexMap.empty;
    edges_from = VertexIndexMap.empty;
  }

let edges_from (graph : graph) (vertex : Vertex.t) (index : index) : EdgeSet.t =
  Option.value
    (VertexIndexMap.find_opt (vertex, index) graph.edges_from)
    ~default:EdgeSet.empty

let edges_to (graph : graph) (vertex : Vertex.t) : EdgeSet.t =
  Option.value (VertexMap.find_opt vertex graph.edges_to) ~default:EdgeSet.empty

let update_edge (graph : graph) (edge : Edge.t) (edge_state : edge_state) :
    graph =
  let old_state = EdgeMap.find_opt edge graph.edge_state in
  let action : edge_state option =
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
      (* let edges : edge UuidMap.t = UuidMap.add edge.id edge graph.edges in *)
      let edge_state : edge_state EdgeMap.t =
        EdgeMap.add edge edge_state graph.edge_state
      in
      let old_edges_to : EdgeSet.t =
        Option.value
          (VertexMap.find_opt (Uuid.unwrap edge).target graph.edges_to)
          ~default:EdgeSet.empty
      in
      let edges_to : EdgeSet.t VertexMap.t =
        VertexMap.add (Uuid.unwrap edge).target
          (EdgeSet.add edge old_edges_to)
          graph.edges_to
      in
      let old_edges_from : EdgeSet.t =
        Option.value
          (VertexIndexMap.find_opt
             ((Uuid.unwrap edge).source, (Uuid.unwrap edge).index)
             graph.edges_from)
          ~default:EdgeSet.empty
      in
      let edges_from : EdgeSet.t VertexIndexMap.t =
        VertexIndexMap.add
          ((Uuid.unwrap edge).source, (Uuid.unwrap edge).index)
          (EdgeSet.add edge old_edges_from)
          graph.edges_from
      in
      (* TODO: short circuit if deleting a non-existant *)
      { (* edges; *) edge_state; edges_to; edges_from }
  | Some Destroyed -> (
      let edge_state : edge_state EdgeMap.t =
        EdgeMap.add edge Destroyed graph.edge_state
      in
      match old_state with
      | None -> { graph with edge_state }
      | _ ->
          let old_edges_to : EdgeSet.t =
            Option.value
              (VertexMap.find_opt (Uuid.unwrap edge).target graph.edges_to)
              ~default:EdgeSet.empty
          in
          let old_edges_to' : EdgeSet.t =
            EdgeSet.filter (( == ) edge) old_edges_to
          in
          let edges_to =
            VertexMap.add (Uuid.unwrap edge).target old_edges_to' graph.edges_to
          in
          let old_edges_from : EdgeSet.t =
            Option.value
              (VertexIndexMap.find_opt
                 ((Uuid.unwrap edge).source, (Uuid.unwrap edge).index)
                 graph.edges_from)
              ~default:EdgeSet.empty
          in
          let old_edges_from' : EdgeSet.t =
            EdgeSet.filter (( == ) edge) old_edges_from
          in
          let edges_from : EdgeSet.t VertexIndexMap.t =
            VertexIndexMap.add
              ((Uuid.unwrap edge).target, (Uuid.unwrap edge).index)
              old_edges_from' graph.edges_from
          in
          { graph with edges_to; edges_from } )
