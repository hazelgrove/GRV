type t = ZExp of ZExp.t | ZPat of ZPat.t | ZTyp of ZTyp.t

let erase_cursor : t -> Term.t = function
  | ZExp ze -> Exp (ZExp.erase_cursor ze)
  | ZPat zp -> Pat (ZPat.erase_cursor zp)
  | ZTyp zt -> Typ (ZTyp.erase_cursor zt)

module Set = struct
  include Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)

  let erase_cursor' (zterms : t) : Term.Set.t =
    elements zterms |> List.map erase_cursor |> Term.Set.of_list

  let erase_cursor = erase_cursor'
end
