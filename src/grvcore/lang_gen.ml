(* TODO: generate parts of viz.ml and view.ml from this *)

type sort_name = string

type constructor_name = string

type index_name = string

type typ = string

type graphviz_string_of = string

type show_string_of = string

type show =
  | String of string
  | Arg of (* zero-based index *) int * show_string_of
  | Index of index_name

(*          [Chars "lam"; Arg "Int.to_string" 0; Child 0; chars ":"],*)

(* TODO: variable name: ts -> ps? *)
type arg = Arg of (typ * graphviz_string_of)

type sort = (sort_name * constructor list) Lazy.t

(* TODO: reorder *)
and constructor =
  | Constructor of
      ( constructor_name
      * arg list
      * (* default *)
      index_name option
      * (* down *)
      index_name option
      * index list
      * show list )

and index = Index of (index_name * sort)

(* Note that down and default_index can't actually happen on Root_root *)
let rec root =
  lazy
    ( "Root",
      [
        Constructor
          ( "root",
            [],
            Some "root",
            Some "root",
            [ Index ("root", exp) ],
            [ Index "root" ] );
      ] )

and exp =
  lazy
    ( "Exp",
      [
        Constructor
          ( "var",
            [ Arg ("string", "(fun x -> x)") ],
            None,
            None,
            [],
            [ Arg (0, "(fun x -> x)") ] );
        Constructor
          ( "lam",
            [],
            Some "param",
            Some "body",
            [
              Index ("param", pat);
              Index ("param_type", typ);
              Index ("body", exp);
            ],
            [
              String "(λ";
              Index "param";
              String ":";
              Index "param_type";
              String ".";
              Index "body";
              String ")";
            ] );
        Constructor
          ( "app",
            [],
            Some "fun",
            Some "fun",
            [ Index ("fun", exp); Index ("arg", exp) ],
            [ String "("; Index "fun"; String " "; Index "arg"; String ")" ] );
        Constructor
          ( "num",
            [ Arg ("int", "Int.to_string") ],
            None,
            None,
            [],
            [ Arg (0, "Int.to_string") ] );
        Constructor
          ( "plus",
            [],
            Some "left",
            Some "left",
            [ Index ("left", exp); Index ("right", exp) ],
            [ Index "left"; String "+"; Index "right" ] );
        (* TODO: sums and pairs *)
      ] )

and pat =
  lazy
    ( "Pat",
      [
        Constructor
          ( "var",
            [ Arg ("string", "(fun x -> x)") ],
            None,
            None,
            [],
            [ Arg (0, "(fun x -> x)") ] );
      ] )

and typ =
  lazy
    ( "Typ",
      [
        Constructor ("num", [], None, None, [], [ String "Num" ]);
        Constructor
          ( "arrow",
            [],
            Some "arg",
            Some "arg",
            [ Index ("arg", typ); Index ("result", typ) ],
            [ Index "arg"; String "→"; Index "result" ] );
      ] )

let sorts = [ root; exp; pat; typ ]

(* TODO: pretty print data *)

(**** Helpers for mode ideomatic code ****)
let f : ('a, unit, string) format -> 'a = Printf.sprintf

let cat ?(join : string option) ?(empty : string option) (f : 'a -> string)
    (l : 'a list) : string =
  let join = Option.value ~default:"" join in
  match (empty, l) with
  | Some empty, [] -> empty
  | _ -> String.concat join (List.map f l)

let arg_pat (ts : arg list) : string = match ts with [] -> "" | _ -> " _"

let bindings (ts : arg list) =
  match ts with
  | [] -> ""
  | _ -> f " (%s)" (String.concat ", " (List.mapi (fun i _ -> f "arg%d" i) ts))

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
    let mk_arg (Arg (t, _)) : string = t in
    let mk_constructor s (Constructor (c, ts, _, _, _, _)) : string =
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
    let mk_constructor s (Constructor (c, ts, _, _, _, _)) : string =
      f "\n    | %s_%s%s -> %s" s c (arg_pat ts) s
    in
    let mk_sort ((lazy (s, cs)) : sort) : string =
      f "\n    (**** %s ****)" s ^ cat (mk_constructor s) cs
    in
    cat mk_sort sorts
  in
  (**** Function body for `graphviz_label` ****)
  let graphviz_label : string =
    let mk_constructor s (Constructor (c, ts, _, _, _, _)) : string =
      let args =
        match ts with
        | [] -> ""
        | _ ->
            f "(%s)"
              (String.concat ", "
                 (List.mapi
                    (fun i (Arg (_, graphviz_string_of)) ->
                      f "\" ^ %s arg%d ^ \"" graphviz_string_of i)
                    ts))
      in
      f "\n    | %s_%s%s -> \"%s_%s%s\"" s c (bindings ts) s c args
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

  (* Specifies the label to use in graphviz *)
  let graphviz_label (c : t) : string =
    match c with%s
end
|}
    t sort_of graphviz_label

(* TODO: generate of_vertex *)
(* TODO: do not run a runtime *)
(* TODO: rename Index to Field, Child_index, child position, child slot? *)
(**** Module: Index ****)
let () =
  (**** Type declaration ****)
  let t =
    let mk_index s c (Index (i, _)) : string = f "\n    | %s_%s_%s" s c i in
    let mk_constructor s (Constructor (c, _, _, _, is, _)) : string =
      f "\n    (* %s_%s *)" s c ^ cat ~empty (mk_index s c) is
    in
    let mk_sort (lazy (s, cs)) : string =
      f "\n    (**** %s ****)" s ^ cat (mk_constructor s) cs
    in
    cat mk_sort sorts
  in
  (**** Function body for `short_name` ****)
  let short_name : string =
    let mk_index s c (Index (i, _)) : string =
      f "\n    | %s_%s_%s -> \"%s\"" s c i i
    in
    let mk_constructor s (Constructor (c, _, _, _, is, _)) : string =
      f "\n    (* %s_%s *)" s c ^ cat ~empty (mk_index s c) is
    in
    let mk_sort ((lazy (s, cs)) : sort) : string =
      f "\n    (**** %s ****)" s ^ cat (mk_constructor s) cs
    in
    cat mk_sort sorts
  in
  (**** Function body for `parent_constructor` ****)
  let parent_constructor : string =
    let mk_index s c (Index (i, _)) : string =
      f "\n    | %s_%s_%s -> %s_%s" s c i s c
    in
    let mk_constructor s (Constructor (c, _, _, _, is, _)) : string =
      f "\n    (* %s_%s *)" s c ^ cat ~empty (mk_index s c) is
    in
    let mk_sort ((lazy (s, cs)) : sort) : string =
      f "\n    (**** %s ****)" s ^ cat (mk_constructor s) cs
    in
    cat mk_sort sorts
  in
  (**** Function body for `child_indexes` ****)
  let child_indexes : string =
    let mk_index s c (Index (i, _)) : string = f "%s_%s_%s" s c i in
    let mk_constructor s (Constructor (c, ts, _, _, is, _)) : string =
      f "\n    | %s_%s%s -> [%s]" s c (arg_pat ts)
        (cat ~join:"; " (mk_index s c) is)
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
    let mk_constructor s (Constructor (c, _, _, _, is, _)) : string =
      f "\n    (* %s_%s *)" s c ^ cat ~empty (mk_index s c) is
    in
    let mk_sort ((lazy (s, cs)) : sort) : string =
      f "\n    (**** %s ****)" s ^ cat (mk_constructor s) cs
    in
    cat mk_sort sorts
  in
  (**** Function body for `default_index` ****)
  let default_index : string =
    let mk_constructor sort (Constructor (c, ts, def, _, _, _)) : string =
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
    let mk_constructor sort (Constructor (c, ts, _, down, _, _)) : string =
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
    let mk_constructor s (Constructor (c, _, _, _, is, _)) : string =
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
    let mk_constructor s (Constructor (c, _, _, _, is, _)) : string =
      f "\n    (* %s_%s *)" s c ^ cat ~empty (mk_index s c) (shift_backward is)
    in
    let mk_sort ((lazy (s, cs)) : sort) : string =
      f "\n    (**** %s ****)" s ^ cat (mk_constructor s) cs
    in
    cat mk_sort sorts
  in
  (**** Function body for `show` ****)
  let show : string =
    let mk_show s c (show : show) : string =
      match show with
      | String s -> f "string %S" s
      | Arg (i, s) -> f "arg (%s arg%d)" s i
      | Index i -> f "index %s_%s_%s" s c i
    in
    let mk_constructor s (Constructor (c, ts, _, _, _, ss)) : string =
      f "\n    | %s_%s%s -> [%s]" s c (bindings ts)
        (cat ~join:"; " (mk_show s c) ss)
    in
    let mk_sort ((lazy (s, cs)) : sort) : string =
      f "\n    (**** %s ****)" s ^ cat (mk_constructor s) cs
    in
    cat mk_sort sorts
  in
  (**** Module declaration ****)
  (* TODO: remove ^ *)
  Printf.printf
    {|
module Index = struct
  type t =%s
  [@@deriving show, eq, ord, sexp_of]

  let short_name (i : t) : string =
    match i with%s

  (* Returns the constructor that parents of a particular index should have *)
  let parent_constructor (i : t) : Constructor.t =
    match i with%s

  (* Returns the constructor that parents of a particular index should have *)
  let child_indexes (c : Constructor.t) : t list =
    match c with%s

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

  (* Specifies how to show the given constructor and its children *)
  let show (string : string -> 'a) (arg : string -> 'a) (index : t -> 'a) (c : Constructor.t) : 'a list =
    match c with%s
end
|}
    t short_name parent_constructor child_indexes child_sort default_index down
    right left show
