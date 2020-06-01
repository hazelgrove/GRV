module List = struct
  let rec intersperse (delim : 'a) (xs : 'a list) : 'a list =
    match xs with
    | [] | [ _ ] -> xs
    | x :: xs' -> x :: delim :: intersperse delim xs'
end

module Option = struct
  (* Note that references to `Option` in this module refer to ocaml's option *)
  module Let_syntax = struct
    module Let_syntax = struct
      let map ~(f : 'a -> 'b) (o : 'a option) : 'b option = Option.map f o

      let bind (o : 'a option) ~(f : 'a -> 'b option) : 'b option =
        Option.bind o f
    end
  end
end
