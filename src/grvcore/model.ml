open Ast
open Ast.HExp
module G = Graph_unsorted

type t = { graph : G.graph; ast : HExp.t; cursor : Cursor.t }

let cutoff (m1 : t) (m2 : t) : bool = m1 == m2

let model graph ast cursor : t = { graph; ast; cursor }

let empty : t = model G.init (Uuid.wrap EmptyHole) Cursor.Here
