open Format

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

  let find (key : Vertex.t) (map : t) : EdgeSet.t =
    Option.value (find_opt key map) ~default:EdgeSet.empty
end

(* Children *)

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

module VertexIndexMap = Map.Make (VertexIndex)

module Children = struct
  include VertexIndexMap

  type t = EdgeSet.t VertexIndexMap.t

  let find (key : key) (map : t) : EdgeSet.t =
    Option.value (find_opt key map) ~default:EdgeSet.empty
end

let child (vertex : Vertex.t) (index : index) : Children.key = { vertex; index }

(* Graph *)

(* Note: edges not in the states field have not been created yet and are `\bot` *)
type t = {
  vertices : Vertex.t UuidMap.t;
  edges : Edge.t UuidMap.t;
  states : Edge.state EdgeMap.t;
  parents : Parents.t;
  children : Children.t;
}

let empty : t =
  {
    vertices : Vertex.t UuidMap.t =
      UuidMap.singleton Vertex.root.uuid Vertex.root;
    edges : Edge.t UuidMap.t = UuidMap.empty;
    states : Edge.state EdgeMap.t = EdgeMap.empty;
    parents : Parents.t = Parents.empty;
    children : Children.t = Children.empty;
  }

(* Graph Pretty Printing *)

let pp_graph (fmt : formatter) (graph : t) : unit =
  fprintf fmt "Vertices\n";
  UuidMap.iter (fun _ v -> fprintf fmt "%a\n" Vertex.pp v) graph.vertices;
  fprintf fmt "\nEdges\n";
  UuidMap.iter (fun id e -> fprintf fmt "%d = %a\n" id Edge.pp e) graph.edges;
  fprintf fmt "\nStates\n";
  EdgeMap.iter
    (fun edge state -> fprintf fmt "%d = %a\n" edge.uuid Edge.pp_state state)
    graph.states;
  fprintf fmt "@?"

(* Graph Operations *)

let find_vertex (vertex : Vertex.t) (graph : t) : Vertex.t =
  UuidMap.find vertex.uuid graph.vertices

let connect_parents (edge : Edge.t) (graph : t) : t =
  let target = Edge.target edge in
  let parents = Parents.find target graph.parents in
  let parents = Parents.add target (EdgeSet.add edge parents) graph.parents in
  { graph with parents }

let disconnect_parents (edge : Edge.t) (graph : t) : t =
  let target = Edge.target edge in
  let parents = Parents.find target graph.parents in
  let parents =
    Parents.add target (EdgeSet.filter (Edge.equal edge) parents) graph.parents
  in
  { graph with parents }

let connect_children (edge : Edge.t) (graph : t) : t =
  let source = child (Edge.source edge) (Edge.index edge) in
  let children = Children.find source graph.children in
  let children =
    Children.add source (EdgeSet.add edge children) graph.children
  in
  { graph with children }

let disconnect_children (edge : Edge.t) (graph : t) : t =
  let source = child (Edge.source edge) (Edge.index edge) in
  let children = Children.find source graph.children in
  let children =
    Children.add source
      (EdgeSet.filter (Edge.equal edge) children)
      graph.children
  in
  { graph with children }

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
  let old_state = EdgeMap.find_opt edge graph.states in
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
      let states : Edge.state EdgeMap.t =
        EdgeMap.add edge Edge.Destroyed graph.states
      in
      match old_state with
      | None -> { graph with vertices; states }
      | _ -> drop_edge { graph with vertices; states } edge )
