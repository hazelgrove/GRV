open Ast
module Graph = Graph_unsorted.Graph

type t = { graph : Graph.t; ast : HExp.t; cursor : Cursor.t }

let cutoff (m1 : t) (m2 : t) : bool = m1 == m2

let model (graph : Graph.t) (ast : HExp.t) (cursor : Cursor.t) : t =
  { graph; ast; cursor }

let empty : t = model Graph.empty (Uuid.wrap HExp.EmptyHole) Cursor.Here
