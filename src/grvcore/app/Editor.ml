type t = {
  id : Uuid.Id.t;
  graph : Graph.t;
  cursor : Cursor.t;
  actions : Graph_action.Set.t;
  known_actions : Graph_action.Set.t;
}

let mk () : t =
  {
    id = Uuid.Id.next ();
    graph = Graph.empty;
    cursor = Cursor.root;
    actions = Graph_action.Set.empty;
    known_actions = Graph_action.Set.empty;
  }

(* module OrderedType = struct
  type nonrec t = t

  let compare : t -> t -> int = Int.compare
end *)

let deleted (editor : t) : Vertex.Set.t =
  Vertex.Set.remove Vertex.root (Roots.roots editor.graph)
