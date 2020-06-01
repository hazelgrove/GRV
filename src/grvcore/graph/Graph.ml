(* Note: edges not in the states field have not been created yet and are `\bot` *)
type t = { states : Edge_state.t Edge.Map.t; cache : Cache.t }

let mk (states : Edge_state.t Edge.Map.t) (cache : Cache.t) : t =
  { states; cache }

let empty : t = mk Edge.Map.empty Cache.empty

let parents (vertex : Vertex.t) (graph : t) : Edge.Set.t =
  Cache.parents vertex graph.cache

let children (cursor : Cursor.t) (graph : t) : Edge.Set.t =
  Cache.children cursor graph.cache
