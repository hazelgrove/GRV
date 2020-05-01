open Format

(* Vertex *)

module Vertex = struct
  type ctor =
    | Root_root
    | Id_id of string
    | Exp_lam
    | Exp_app
    | Exp_var
    | Typ_app
    | Typ_var
  [@@deriving show]

  type t = ctor Uuid.t

  let compare : t -> t -> int = Uuid.compare

  let pp : formatter -> t -> unit = Uuid.pp pp_ctor

  let root : t = { uuid = 0; value = Root_root }
end

let vertex (ctor : Vertex.ctor) : Vertex.t = Uuid.wrap ctor

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
[@@deriving compare, show]

module Edge = struct
  type t = t' Uuid.t [@@deriving show]

  and t' = { source : Vertex.t; index : index; target : Vertex.t }

  type state = Created | Destroyed [@@deriving show]

  let compare : t -> t -> int = Uuid.compare

  let equal (edge1 : t) (edge2 : t) : bool = compare edge1 edge2 = 0

  let source (edge : t) : Vertex.t = (Uuid.unwrap edge).source

  let target (edge : t) : Vertex.t = (Uuid.unwrap edge).target

  let index (edge : t) : index = (Uuid.unwrap edge).index
end

let edge (source : Vertex.t) (index : index) (target : Vertex.t) : Edge.t =
  let e' : Edge.t' = { source; index; target } in
  Uuid.wrap e'

module UuidMap = Map.Make (Uuid.OrderedType)
module EdgeMap = Map.Make (Edge)

module EdgeSet = Set.Make (struct
  type t = Edge.t

  let compare : t -> t -> int = Edge.compare
end)

(* Parents *)

module VertexMap = Map.Make (Vertex)

module Parents = struct
  include VertexMap

  type t = EdgeSet.t VertexMap.t

  let obtain (key : Vertex.t) (map : t) : EdgeSet.t =
    Option.value (find_opt key map) ~default:EdgeSet.empty
end

(* VertexIndexMap *)

module VertexIndex = struct
  type t = { vertex : Vertex.t; index : index }

  let compare (child1 : t) (child2 : t) : int =
    match Vertex.compare child1.vertex child2.vertex with
    | 0 -> compare_index child1.index child2.index
    | i -> i

  let pp (fmt : formatter) (child : t) : unit =
    fprintf fmt "{\n";
    fprintf fmt "  vertex: %a\n" Vertex.pp child.vertex;
    fprintf fmt "   index: %a\n" pp_index child.index;
    fprintf fmt "} "
end

let child (vertex : Vertex.t) (index : index) : VertexIndex.t =
  { vertex; index }

module VertexIndexMap = struct
  include Map.Make (VertexIndex)

  let obtain (key : VertexIndex.t) (map : EdgeSet.t t) : EdgeSet.t =
    Option.value (find_opt key map) ~default:EdgeSet.empty
end

(* Graph *)

type t = {
  (* Maps vertex id to vertex *)
  vertices : Vertex.t UuidMap.t;
  (* Maps edge id to edge *)
  edges : Edge.t UuidMap.t;
  (* Note: edges not in the table have not been created yet and are `\bot` *)
  edge_states : Edge.state EdgeMap.t;
  (* Maps Vertex id to parent edge *)
  edges_to : Parents.t;
  (* Maps Vertex id and Child index to set of edges *)
  edges_from : EdgeSet.t VertexIndexMap.t;
}

let empty : t =
  {
    vertices : Vertex.t UuidMap.t =
      UuidMap.singleton Vertex.root.uuid Vertex.root;
    edges : Edge.t UuidMap.t = UuidMap.empty;
    edge_states : Edge.state EdgeMap.t = EdgeMap.empty;
    edges_to : Parents.t = Parents.empty;
    edges_from : EdgeSet.t VertexIndexMap.t = VertexIndexMap.empty;
  }

(* Graph Pretty Printing *)

let pp_vertices (fmt : formatter) (graph : t) : unit =
  UuidMap.iter (fun _ v -> fprintf fmt "%a\n" Vertex.pp v) graph.vertices

let pp_edges (fmt : formatter) (graph : t) : unit =
  UuidMap.iter (fun id e -> fprintf fmt "%d = %a\n" id Edge.pp e) graph.edges

let pp_edge_states (fmt : formatter) (graph : t) : unit =
  EdgeMap.iter
    (fun edge state -> fprintf fmt "%d = %a\n" edge.uuid Edge.pp_state state)
    graph.edge_states

let pp_graph (fmt : formatter) (graph : t) : unit =
  fprintf fmt "Vertices:\n%a\n" pp_vertices graph;
  fprintf fmt "Edges:\n%a\n" pp_edges graph;
  fprintf fmt "Edge States:\n%a@." pp_edge_states graph

(* Graph Operations *)

let find_vertex (vertex : Vertex.t) (graph : t) : Vertex.t =
  UuidMap.find vertex.uuid graph.vertices

let connect_parents (edge : Edge.t) (graph : t) : t =
  let target = Edge.target edge in
  let parents = Parents.obtain target graph.edges_to in
  let edges_to = Parents.add target (EdgeSet.add edge parents) graph.edges_to in
  { graph with edges_to }

let disconnect_parents (edge : Edge.t) (graph : t) : t =
  let target = Edge.target edge in
  let parents = Parents.obtain target graph.edges_to in
  let edges_to =
    Parents.add target (EdgeSet.filter (Edge.equal edge) parents) graph.edges_to
  in
  { graph with edges_to }

let connect_children (edge : Edge.t) (graph : t) : t =
  let source = child (Edge.source edge) (Edge.index edge) in
  let children = VertexIndexMap.obtain source graph.edges_from in
  let edges_from =
    VertexIndexMap.add source (EdgeSet.add edge children) graph.edges_from
  in
  { graph with edges_from }

let disconnect_children (edge : Edge.t) (graph : t) : t =
  let source = child (Edge.source edge) (Edge.index edge) in
  let children = VertexIndexMap.obtain source graph.edges_from in
  let edges_from =
    VertexIndexMap.add source
      (EdgeSet.filter (Edge.equal edge) children)
      graph.edges_from
  in
  { graph with edges_from }

let add_edge (graph : t) (edge : Edge.t) : t =
  graph |> connect_parents edge |> connect_children edge

let drop_edge (graph : t) (edge : Edge.t) : t =
  graph |> disconnect_parents edge |> disconnect_children edge

let update_edge (graph : t) (edge : Edge.t) (edge_state : Edge.state) : t =
  let vertices =
    let target = Edge.target edge in
    let source = Edge.source edge in
    graph.vertices
    |> UuidMap.add target.uuid target
    |> UuidMap.add source.uuid source
  in
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
      add_edge { graph with vertices } edge
  | Some Destroyed -> (
      let edge_states : Edge.state EdgeMap.t =
        EdgeMap.add edge Edge.Destroyed graph.edge_states
      in
      match old_state with
      | None -> { graph with vertices; edge_states }
      | _ -> drop_edge { graph with vertices; edge_states } edge )
