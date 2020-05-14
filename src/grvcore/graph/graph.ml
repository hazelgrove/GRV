(* Note: edges not in the states field have not been created yet and are `\bot` *)
(* TODO: split live_parents and live_children from all_parents and all_children fields *)
type t = {
  edges : Edge.t Uuid.Map.t;
  states : Edge_state.t Edge.Map.t;
  cache : Cache.t;
}

let mk (edges : Edge.t Uuid.Map.t) (states : Edge_state.t Edge.Map.t)
    (cache : Cache.t) : t =
  { edges; states; cache }

(* TODO: better field names *)

let empty : t = mk Uuid.Map.empty Edge.Map.empty Cache.empty

let edge_is_live (graph : t) (edge : Edge.t) : bool =
  Some Edge_state.Created = Edge.Map.find_opt edge graph.states

let pp (fmt : Format.formatter) (graph : t) : unit =
  let open Format in
  Cache.pp fmt graph.cache;
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
