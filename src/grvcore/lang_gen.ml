(* TODO: generate parts of viz.ml and view.ml from this *)

type index_name = string

type sort_name = string

type constructor_name = string

type typ = string

(* TODO: point directly to sort? *)
type index = Index of (index_name * sort)

and constructor =
  | Constructor of
      ( constructor_name
      * typ list
      * (* default *)
      index_name option
      * (* down *)
      index_name option
      * index list )

and sort = (sort_name * constructor list) Lazy.t

(* Note that down and default_index can't actually happen on Root_root *)
let rec root =
  lazy
    ( "Root",
      [
        Constructor
          ("root", [], Some "root", Some "root", [ Index ("root", exp) ]);
      ] )

and exp =
  lazy
    ( "Exp",
      [
        Constructor ("var", [ "string" ], None, None, []);
        Constructor
          ( "lam",
            [],
            Some "param",
            Some "body",
            [
              Index ("param", pat);
              Index ("param_type", typ);
              Index ("body", exp);
            ] );
        Constructor
          ( "app",
            [],
            Some "fun",
            Some "fun",
            [ Index ("fun", exp); Index ("arg", exp) ] );
        Constructor ("num", [ "int" ], None, None, []);
        Constructor
          ( "plus",
            [],
            Some "left",
            Some "left",
            [ Index ("left", exp); Index ("right", exp) ] );
        (* TODO: sums and pairs *)
      ] )

and pat = lazy ("Pat", [ Constructor ("var", [ "string" ], None, None, []) ])

and typ =
  lazy
    ( "Typ",
      [
        Constructor ("num", [], None, None, []);
        Constructor
          ( "arrow",
            [],
            Some "arg",
            Some "arg",
            [ Index ("arg", typ); Index ("result", typ) ] );
      ] )

let sorts = [ root; exp; pat; typ ]

(* TODO: pretty print data *)
(* TODO: deriving compare vs deriving ord *)

(**** Helpers for mode ideomatic code ****)
let f : ('a, unit, string) format -> 'a = Printf.sprintf

let cat ?(join : string option) ?(empty : string option) (f : 'a -> string)
    (l : 'a list) : string =
  let join = Option.value ~default:"" join in
  match (empty, l) with
  | Some empty, [] -> empty
  | _ -> String.concat join (List.map f l)

let arg_pat (ts : typ list) : string = match ts with [] -> "" | _ -> " _"

let shift_forward (l : 'a list) : ('b * 'b option) list =
  let l2 = List.tl (List.map (fun s -> Some s) l @ [ None ]) in
  List.map2 (fun x y -> (x, y)) l l2

let shift_backward (l : 'a list) : ('b * 'b option) list =
  let l2 =
    List.rev (List.tl (List.rev (None :: List.map (fun s -> Some s) l)))
  in
  List.map2 (fun x y -> (x, y)) l l2

let empty = "\n    (* -- empty -- *)"

(**** Module: Sort ****)
let () =
  (**** Type declaration ****)
  let t : string =
    let mk_sort ((lazy (s, _)) : sort) : string = f "\n    | %s" s in
    cat mk_sort sorts
  in
  (**** Module declaration ****)
  Printf.printf
    {|
module Sort = struct
  type t = %s
  [@@deriving show, eq, ord, sexp_of]
end
|}
    t

(**** Module: Constructor ****)
let () =
  (*** Type declaration ****)
  let t : string =
    let mk_arg (t : typ) : string = t in
    let mk_constructor s (Constructor (c, ts, _, _, _)) : string =
      let args =
        match ts with [] -> "" | _ -> f " of (%s)" (cat ~join:", " mk_arg ts)
      in
      f "\n    | %s_%s%s" s c args
    in
    let mk_sort ((lazy (s, cs)) : sort) : string =
      f "\n    (**** %s ****)" s ^ cat (mk_constructor s) cs
    in
    cat mk_sort sorts
  in
  (**** Function body for `sort_of` ****)
  let sort_of : string =
    let mk_constructor s (Constructor (c, ts, _, _, _)) : string =
      f "\n    | %s_%s%s -> %s" s c (arg_pat ts) s
    in
    let mk_sort ((lazy (s, cs)) : sort) : string =
      f "\n    (**** %s ****)" s ^ cat (mk_constructor s) cs
    in
    cat mk_sort sorts
  in
  (**** Module declaration ****)
  Printf.printf
    {|
module Constructor = struct
  open Sexplib0.Sexp_conv

  type t =%s
  [@@deriving show, eq, ord, sexp_of]

  (* Returns the sort of a particular constructor *)
  let sort_of (c : t) : Sort.t =
    match c with%s
end
|}
    t sort_of

(* TODO: rename Index to Field, Child_index, child position, child slot? *)
(**** Module: Index ****)
let () =
  (**** Type declaration ****)
  let t =
    let mk_index s c (Index (i, _)) : string = f "\n    | %s_%s_%s" s c i in
    let mk_constructor s (Constructor (c, _, _, _, is)) : string =
      f "\n    (* %s_%s *)" s c ^ cat ~empty (mk_index s c) is
    in
    let mk_sort (lazy (s, cs)) : string =
      f "\n    (**** %s ****)" s ^ cat (mk_constructor s) cs
    in
    cat mk_sort sorts
  in
  (**** Function body for `parent_constructor` ****)
  let parent_constructor : string =
    let mk_index s c (Index (i, _)) : string =
      f "\n    | %s_%s_%s -> %s_%s" s c i s c
    in
    let mk_constructor s (Constructor (c, _, _, _, is)) : string =
      f "\n    (* %s_%s *)" s c ^ cat ~empty (mk_index s c) is
    in
    let mk_sort ((lazy (s, cs)) : sort) : string =
      f "\n    (**** %s ****)" s ^ cat (mk_constructor s) cs
    in
    cat mk_sort sorts
  in
  (**** Function body for `child_sort` ****)
  let child_sort : string =
    let mk_index s c (Index (i, (lazy (s_i, _)))) : string =
      f "\n    | %s_%s_%s -> %s" s c i s_i
    in
    let mk_constructor s (Constructor (c, _, _, _, is)) : string =
      f "\n    (* %s_%s *)" s c ^ cat ~empty (mk_index s c) is
    in
    let mk_sort ((lazy (s, cs)) : sort) : string =
      f "\n    (**** %s ****)" s ^ cat (mk_constructor s) cs
    in
    cat mk_sort sorts
  in
  (**** Function body for `default_index` ****)
  let default_index : string =
    let mk_constructor sort (Constructor (c, ts, def, _, _)) : string =
      f "\n    | %s_%s%s -> " sort c (arg_pat ts)
      ^ match def with None -> "None" | Some d -> f "Some %s_%s_%s" sort c d
    in
    let mk_sort ((lazy (s, cs)) : sort) : string =
      f "\n    (**** %s ****)" s ^ cat (mk_constructor s) cs
    in
    cat mk_sort sorts
  in
  (**** Function body for `down` ****)
  let down : string =
    let mk_constructor sort (Constructor (c, ts, _, down, _)) : string =
      f "\n    | %s_%s%s -> " sort c (arg_pat ts)
      ^ match down with None -> "None" | Some d -> f "Some %s_%s_%s" sort c d
    in
    let mk_sort ((lazy (s, cs)) : sort) : string =
      f "\n    (**** %s ****)" s ^ cat (mk_constructor s) cs
    in
    cat mk_sort sorts
  in
  (**** Function body for `right` ****)
  let right : string =
    let mk_index s c (Index (i1, _), i2) : string =
      match i2 with
      | None -> f "\n    | %s_%s_%s -> None" s c i1
      | Some (Index (i2, _)) ->
          f "\n    | %s_%s_%s -> Some %s_%s_%s" s c i1 s c i2
    in
    let mk_constructor s (Constructor (c, _, _, _, is)) : string =
      f "\n    (* %s_%s *)" s c ^ cat ~empty (mk_index s c) (shift_forward is)
    in
    let mk_sort ((lazy (s, cs)) : sort) : string =
      f "\n    (**** %s ****)" s ^ cat (mk_constructor s) cs
    in
    cat mk_sort sorts
  in
  (**** Function body for `left` ****)
  let left : string =
    let mk_index s c (Index (i1, _), i2) : string =
      match i2 with
      | None -> f "\n    | %s_%s_%s -> None" s c i1
      | Some (Index (i2, _)) ->
          f "\n    | %s_%s_%s -> Some %s_%s_%s" s c i1 s c i2
    in
    let mk_constructor s (Constructor (c, _, _, _, is)) : string =
      f "\n    (* %s_%s *)" s c ^ cat ~empty (mk_index s c) (shift_backward is)
    in
    let mk_sort ((lazy (s, cs)) : sort) : string =
      f "\n    (**** %s ****)" s ^ cat (mk_constructor s) cs
    in
    cat mk_sort sorts
  in
  (**** Module declaration ****)
  Printf.printf
    {|
module Index = struct
  type t =%s
  [@@deriving show, eq, ord, sexp_of]

  (* Returns the constructor that parents of a particular index should have *)
  let parent_constructor (i : t) : Constructor.t =
    match i with%s

  (* Returns the sort that children of a particular index should have *)
  let child_sort (i : t) : Sort.t =
    match i with%s

  (* Specifies where the existing node goes when a node is wrapped by the given constructor *)
  let default_index (c : Constructor.t) : t option =
    match c with%s

  (* Specifies where to go when the cursor moves down *)
  let down (c : Constructor.t) : t option =
    match c with%s

  (* Specifies where to go when the cursor moves right *)
  let right (i : t) : t option =
    match i with%s

  (* Specifies where to go when the cursor moves left *)
  let left (i : t) : t option =
    match i with%s
end
|}
    t parent_constructor child_sort default_index down right left
