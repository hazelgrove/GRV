let of_map (bindings : ('k * 'v) list) (sexp_of_key : 'k -> Sexplib.Sexp.t)
    (sexp_of_value : 'v -> Sexplib.Sexp.t) : Sexplib.Sexp.t =
  Sexplib.Std.sexp_of_list
    (fun (k, v) -> Sexplib.Sexp.List [ sexp_of_key k; sexp_of_value v ])
    bindings

(* let of_list (elements : 'a list) (sexp_of_element : 'a -> Sexplib.Sexp.t) :
 *     Sexplib.Sexp.t =
 *   Sexplib.Std.sexp_of_list sexp_of_element elements
 * 
 * let to_list (sexp : Sexplib.Sexp.t) : 'a list *)
