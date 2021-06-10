module T = struct
  type t = Var of string

  let compare = compare

  let constructor : t -> GroveLang.constructor = function
    | Var name -> PatVar name
end

include Sort.Make (T)
