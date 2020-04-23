open Ast
open Ast.HExp

type t = { ast : HExp.t; cursor : Cursor.t }

let cutoff (m1 : t) (m2 : t) : bool = m1 == m2

let init () : t = { ast = Uuid.wrap EmptyHole; cursor = Cursor.Here }
