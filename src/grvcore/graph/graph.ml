type cache = {
  vertices : Vertex.t Uuid.Map.t;
  parents : Edge.Set.t Vertex.Map.t;
  children : Edge.Set.t Cursor.Map.t;
}

(* Note: edges not in the states field have not been created yet and are `\bot` *)
(* TODO: split live_parents and live_children from all_parents and all_children fields *)
type t = {
  edges : Edge.t Uuid.Map.t;
  states : Edge_state.t Edge.Map.t;
  cache : cache;
}

let mk_cache (vertices : Vertex.t Uuid.Map.t)
    (parents : Edge.Set.t Vertex.Map.t) (children : Edge.Set.t Cursor.Map.t) :
    cache =
  { vertices; parents; children }

let mk (edges : Edge.t Uuid.Map.t) (states : Edge_state.t Edge.Map.t)
    (cache : cache) : t =
  { edges; states; cache }

(* TODO: better field names *)

let empty_cache : cache =
  mk_cache
    (Uuid.Map.singleton Vertex.root.id Vertex.root)
    Vertex.Map.empty Cursor.Map.empty

let empty : t = mk Uuid.Map.empty Edge.Map.empty empty_cache

let edge_is_live (graph : t) (edge : Edge.t) : bool =
  Some Edge_state.Created = Edge.Map.find_opt edge graph.states

let find_vertex (vertex : Vertex.t) (graph : t) : Vertex.t =
  Uuid.Map.find vertex.id graph.cache.vertices

let find_children (cursor : Cursor.t) (graph : t) : Edge.Set.t =
  Edge.Set.filter (edge_is_live graph)
  @@ Option.value
       (Cursor.Map.find_opt cursor graph.cache.children)
       ~default:Edge.Set.empty

let find_parents (vertex : Vertex.t) (graph : t) : Edge.Set.t =
  Edge.Set.filter (edge_is_live graph)
  @@ Option.value
       (Vertex.Map.find_opt vertex graph.cache.parents)
       ~default:Edge.Set.empty

let pp_graph (fmt : Format.formatter) (graph : t) : unit =
  let open Format in
  fprintf fmt "Vertices\n";
  Uuid.Map.iter (fun _ v -> fprintf fmt "%a\n" Vertex.pp v) graph.cache.vertices;
  fprintf fmt "\nEdges\n";
  Uuid.Map.iter
    (fun id e -> fprintf fmt "%s = %a\n" (Uuid.Id.show id) Edge.pp e)
    graph.edges;
  fprintf fmt "\nStates\n";
  Edge.Map.iter
    (fun edge state ->
      fprintf fmt "%s = %a\n" (Uuid.Id.show edge.id) Edge_state.pp state)
    graph.states;
  fprintf fmt "@?"
