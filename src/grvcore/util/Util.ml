module List = struct
  let rec intersperse (delim : 'a) (xs : 'a list) : 'a list =
    match xs with
    | [] | [ _ ] -> xs
    | x :: xs' -> x :: delim :: intersperse delim xs'
end

module Sexp = struct
  let print (sexp : Sexplib0.Sexp.t) : unit =
    Sexplib0.Sexp.to_string_hum sexp |> print_endline

  let of_map (bindings : ('k * 'v) list) (sexp_of_key : 'k -> Sexplib.Sexp.t)
      (sexp_of_value : 'v -> Sexplib.Sexp.t) : Sexplib.Sexp.t =
    Sexplib.Std.sexp_of_list
      (fun (k, v) -> Sexplib.Sexp.List [ sexp_of_key k; sexp_of_value v ])
      bindings
end
