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

type vertex = { id : Uuid.t; ctor : ctor } [@@deriving compare]

type edge = { id : Uuid.t; source : vertex; index : index; target : vertex }
[@@deriving compare]

type edge_state = Created | Destroyed

module UuidMap = Map.Make (Uuid)

module UuidIndexOrderedType = struct
  type t = Uuid.t * index

  let compare ((u1, i1) : t) ((u2, i2) : t) : int =
    match compare u1 u2 with 0 -> compare_index i1 i2 | cmp -> cmp
end

module EdgeOrderedType = struct
  type t = edge

  let compare : t -> t -> int = compare_edge
end

module UuidIndexMap = Map.Make (UuidIndexOrderedType)
module EdgeSet = Set.Make (EdgeOrderedType)

type graph = {
  (* Maps Edge id to edge *)
  edges : edge UuidMap.t;
  (* Note: edges not in the table have not been created yet and are `\bot` *)
  edge_state : edge_state UuidMap.t;
  (* Maps Vertex id to parent edge *)
  edges_to : EdgeSet.t UuidMap.t;
  (* Maps Vertex id and Child index to set of edges *)
  edges_from : EdgeSet.t UuidIndexMap.t;
}

let init : graph =
  {
    edges = UuidMap.empty;
    edge_state = UuidMap.empty;
    edges_to = UuidMap.empty;
    edges_from = UuidIndexMap.empty;
  }

let edges_from : graph -> Uuid.t -> index -> EdgeSet.t =
 fun graph vertex index ->
  match UuidIndexMap.find_opt (vertex, index) graph.edges_from with
  | None -> EdgeSet.empty
  | Some edges -> edges

let edges_to : graph -> Uuid.t -> EdgeSet.t =
 fun graph vertex ->
  match UuidMap.find_opt vertex graph.edges_to with
  | None -> EdgeSet.empty
  | Some edges -> edges

let update_edge : graph -> edge -> edge_state -> graph =
 fun graph edge edge_state ->
  let old_state = UuidMap.find_opt edge.id graph.edge_state in
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
      let edges : edge UuidMap.t = UuidMap.add edge.id edge graph.edges in
      let edge_state : edge_state UuidMap.t =
        UuidMap.add edge.id edge_state graph.edge_state
      in
      let old_edges_to : EdgeSet.t =
        Option.value
          (UuidMap.find_opt edge.target.id graph.edges_to)
          ~default:EdgeSet.empty
      in
      let edges_to : EdgeSet.t UuidMap.t =
        UuidMap.add edge.target.id
          (EdgeSet.add edge old_edges_to)
          graph.edges_to
      in
      let old_edges_from : EdgeSet.t =
        Option.value
          (UuidIndexMap.find_opt (edge.source.id, edge.index) graph.edges_from)
          ~default:EdgeSet.empty
      in
      let edges_from : EdgeSet.t UuidIndexMap.t =
        UuidIndexMap.add
          (edge.source.id, edge.index)
          (EdgeSet.add edge old_edges_from)
          graph.edges_from
      in
      (* TODO: short circuit if deleting a non-existant *)
      { edges; edge_state; edges_to; edges_from }
  | Some Destroyed -> (
      let edge_state : edge_state UuidMap.t =
        UuidMap.add edge.id Destroyed graph.edge_state
      in
      match old_state with
      | None -> { graph with edge_state }
      | _ ->
          let old_edges_to : EdgeSet.t =
            Option.value
              (UuidMap.find_opt edge.target.id graph.edges_to)
              ~default:EdgeSet.empty
          in
          let old_edges_to' : EdgeSet.t =
            EdgeSet.filter (( == ) edge) old_edges_to
          in
          let edges_to =
            UuidMap.add edge.target.id old_edges_to' graph.edges_to
          in
          let old_edges_from : EdgeSet.t =
            Option.value
              (UuidIndexMap.find_opt
                 (edge.source.id, edge.index)
                 graph.edges_from)
              ~default:EdgeSet.empty
          in
          let old_edges_from' : EdgeSet.t =
            EdgeSet.filter (( == ) edge) old_edges_from
          in
          let edges_from : EdgeSet.t UuidIndexMap.t =
            UuidIndexMap.add
              (edge.target.id, edge.index)
              old_edges_from' graph.edges_from
          in
          { graph with edges_to; edges_from } )
