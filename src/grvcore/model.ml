open Ast

type t = {
  graph : Graph.t;
  ast : HExp.t;
  cursor : Cursor.t;
  cursor_ref : Graph.VertexIndex.t;
}

let cutoff (m1 : t) (m2 : t) : bool = m1 == m2

let model (graph : Graph.t) (ast : HExp.t) (cursor : Cursor.t) : t =
  let cursor_ref = Graph.child Vertex.root Graph.Root_root_root in
  { graph; ast; cursor; cursor_ref }

let empty : t = model Graph.empty (Uuid.wrap HExp.EmptyHole) Cursor.Here
