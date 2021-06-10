module T = struct
  type t = Var of string | Lam of Pat.t * Typ.t * t | App of t * t

  let compare = compare

  let constructor : t -> GroveLang.constructor = function
    | Var name -> ExpVar name
    | Lam (_, _, _) -> ExpLam
    | App (_, _) -> ExpApp
end

include Sort.Make (T)
