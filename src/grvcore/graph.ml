open Format

module Parents = struct
  include Vertex.Map

  type t = Edge.Set.t Vertex.Map.t

  let find (key : Vertex.t) (map : t) : Edge.Set.t =
    Option.value (find_opt key map) ~default:Edge.Set.empty
end

(* Children *)

module Child = struct
  type t = { vertex : Vertex.t; index : Index.t }

  let compare (child1 : t) (child2 : t) : int =
    match Vertex.compare child1.vertex child2.vertex with
    | 0 -> Index.compare child1.index child2.index
    | i -> i

  let pp (fmt : formatter) (child : t) : unit =
    fprintf fmt "{\n";
    fprintf fmt "  vertex: %a\n" Vertex.pp child.vertex;
    fprintf fmt "   index: %a\n" Index.pp child.index;
    fprintf fmt "} "
end

module ChildMap = Map.Make (Child)

module Children = struct
  include ChildMap

  type t = Edge.Set.t ChildMap.t

  let find (key : key) (map : t) : Edge.Set.t =
    Option.value (find_opt key map) ~default:Edge.Set.empty
end

let child (vertex : Vertex.t) (index : Index.t) : Children.key =
  { vertex; index }

(* Graph *)

(* Note: edges not in the states field have not been created yet and are `\bot` *)
type t = {
  vertices : Vertex.t Uuid.Map.t;
  edges : Edge.t Uuid.Map.t;
  states : Edge.state Edge.Map.t;
  parents : Parents.t;
  children : Children.t;
}

let empty : t =
  {
    vertices : Vertex.t Uuid.Map.t =
      Uuid.Map.singleton Vertex.root.uuid Vertex.root;
    edges : Edge.t Uuid.Map.t = Uuid.Map.empty;
    states : Edge.state Edge.Map.t = Edge.Map.empty;
    parents : Parents.t = Parents.empty;
    children : Children.t = Children.empty;
  }

(* Graph Operations *)

let find_vertex (vertex : Vertex.t) (graph : t) : Vertex.t =
  Uuid.Map.find vertex.uuid graph.vertices

let connect_parents (edge : Edge.t) (graph : t) : t =
  let target = Edge.target edge in
  let parents = Parents.find target graph.parents in
  let parents = Parents.add target (Edge.Set.add edge parents) graph.parents in
  { graph with parents }

let disconnect_parents (edge : Edge.t) (graph : t) : t =
  let target = Edge.target edge in
  let parents = Parents.find target graph.parents in
  let parents =
    Parents.add target (Edge.Set.filter (Edge.equal edge) parents) graph.parents
  in
  { graph with parents }

let connect_children (edge : Edge.t) (graph : t) : t =
  let source = child (Edge.source edge) (Edge.index edge) in
  let children = Children.find source graph.children in
  let children =
    Children.add source (Edge.Set.add edge children) graph.children
  in
  { graph with children }

let disconnect_children (edge : Edge.t) (graph : t) : t =
  let source = child (Edge.source edge) (Edge.index edge) in
  let children = Children.find source graph.children in
  let children =
    Children.add source
      (Edge.Set.filter (Edge.equal edge) children)
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
    |> Uuid.Map.add target.uuid target
    |> Uuid.Map.add source.uuid source
  in
  let old_state = Edge.Map.find_opt edge graph.states in
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
      let states : Edge.state Edge.Map.t =
        Edge.Map.add edge Edge.Destroyed graph.states
      in
      match old_state with
      | None -> { graph with vertices; states }
      | _ -> drop_edge { graph with vertices; states } edge )

(* Pretty Printing *)

let pp_graph (fmt : formatter) (graph : t) : unit =
  fprintf fmt "Vertices\n";
  Uuid.Map.iter (fun _ v -> fprintf fmt "%a\n" Vertex.pp v) graph.vertices;
  fprintf fmt "\nEdges\n";
  Uuid.Map.iter (fun id e -> fprintf fmt "%d = %a\n" id Edge.pp e) graph.edges;
  fprintf fmt "\nStates\n";
  Edge.Map.iter
    (fun edge state -> fprintf fmt "%d = %a\n" edge.uuid Edge.pp_state state)
    graph.states;
  fprintf fmt "@?"
