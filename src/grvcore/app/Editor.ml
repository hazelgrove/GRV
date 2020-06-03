type t' = {
  graph : Graph.t;
  cursor : Cursor.t;
  actions : Graph_action.t list; (* TODO: known_actions : Graph_action.Set.t *)
}

type t = t' Uuid.Wrap.t

(* let cutoff (e1 : t) (e2 : t) : bool = m1 == m2 *)

let mk () : t =
  Uuid.Wrap.mk { graph = Graph.empty; cursor = Cursor.root; actions = [] }

module OrderedType = struct
  type nonrec t = t

  let compare : t -> t -> int = Uuid.Wrap.compare
end

let deleted (editor : t) : Vertex.Set.t =
  Vertex.Set.remove Vertex.root (Roots.roots editor.value.graph)
