type t = { counter : int }

let cutoff (m1 : t) (m2 : t) : bool = m1 == m2

let init () : t = { counter = 0 }
