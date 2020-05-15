(* Note: edges not in the states field have not been created yet and are `\bot` *)
(* TODO: split live_parents and live_children from all_parents and all_children fields *)
type t = { states : Edge_state.t Edge.Map.t; cache : Cache.t }

let mk (states : Edge_state.t Edge.Map.t) (cache : Cache.t) : t =
  { states; cache }

let empty : t = mk Edge.Map.empty Cache.empty

let pp (fmt : Format.formatter) (graph : t) : unit =
  let open Format in
  Cache.pp fmt graph.cache;
  fprintf fmt "\nStates\n";
  Edge.Map.iter
    (fun edge state ->
      fprintf fmt "%s = %a\n" (Uuid.Id.show edge.id) Edge_state.pp state)
    graph.states;
  fprintf fmt "@?"
