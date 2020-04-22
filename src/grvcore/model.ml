type t = { ast : Ast.HExp.t; cursor : Cursor.t }

let cutoff (m1 : t) (m2 : t) : bool = m1 == m2

let init () : t = { ast = Uuid.wrap Ast.HExp.EmptyHole; cursor = [] }
