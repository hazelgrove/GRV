type t = Model

let cutoff (m1 : t) (m2 : t) : bool = Core_kernel.phys_equal m1 m2

let init () : t = Model
