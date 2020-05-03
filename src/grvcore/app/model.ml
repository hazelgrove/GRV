type t = { graph : Graph.t; cursor : Graph.Child.t }

let cutoff (m1 : t) (m2 : t) : bool = m1 == m2

let empty : t = { graph = Graph.empty; cursor = Graph.Child.root }
