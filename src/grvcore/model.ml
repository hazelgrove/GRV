type t = { graph : Graph.t; cursor_ref : Graph.Child.t }

let cutoff (m1 : t) (m2 : t) : bool = m1 == m2

let empty : t = { graph = Graph.empty; cursor_ref = Graph.Child.root }
