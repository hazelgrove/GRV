open Ast
open Ast.HExp

type t = { graph : Graph_unsorted.graph; ast : HExp.t; cursor : Cursor.t }

let cutoff (m1 : t) (m2 : t) : bool = m1 == m2

let init () : t =
  {
    graph = Graph_unsorted.init;
    ast = Uuid.wrap EmptyHole;
    cursor = Cursor.Here;
  }
