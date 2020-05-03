module Parents = struct
  include Vertex.Map

  type t = Edge.Set.t Vertex.Map.t

  let find (key : Vertex.t) (map : t) : Edge.Set.t =
    Option.value (find_opt key map) ~default:Edge.Set.empty
end

(* Children *)

module Child = struct
  type t = { parent : Vertex.t; index : Index.t }

  let compare (child1 : t) (child2 : t) : int =
    match Vertex.compare child1.parent child2.parent with
    | 0 -> Index.compare child1.index child2.index
    | i -> i

  let pp (fmt : Format.formatter) (child : t) : unit =
    let open Format in
    fprintf fmt "{\n";
    fprintf fmt "  vertex: %a\n" Vertex.pp child.parent;
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

let child (parent : Vertex.t) (index : Index.t) : Children.key =
  { parent; index }

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
      Uuid.Map.singleton Vertex.root.id Vertex.root;
    edges : Edge.t Uuid.Map.t = Uuid.Map.empty;
    states : Edge.state Edge.Map.t = Edge.Map.empty;
    parents : Parents.t = Parents.empty;
    children : Children.t = Children.empty;
  }

(* Graph Operations *)

let find_vertex (vertex : Vertex.t) (graph : t) : Vertex.t =
  Uuid.Map.find vertex.id graph.vertices

let find_children (child : Child.t) (graph : t) : Edge.Set.t =
  Children.find child graph.children

(* Pretty Printing *)

let pp_graph (fmt : Format.formatter) (graph : t) : unit =
  let open Format in
  fprintf fmt "Vertices\n";
  Uuid.Map.iter (fun _ v -> fprintf fmt "%a\n" Vertex.pp v) graph.vertices;
  fprintf fmt "\nEdges\n";
  Uuid.Map.iter (fun id e -> fprintf fmt "%d = %a\n" id Edge.pp e) graph.edges;
  fprintf fmt "\nStates\n";
  Edge.Map.iter
    (fun edge state -> fprintf fmt "%d = %a\n" edge.id Edge.pp_state state)
    graph.states;
  fprintf fmt "@?"
