(* TODO: generate keybindings from this? *)

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

(* TODO: variable name: ts -> ps? *)
type arg = typ * graphviz_string_of

type sort = (sort_name * constructor list) Lazy.t

and index = index_name * sort

and constructor = {
  name : constructor_name;
  args : arg list;
  indexes : index list;
  default : index_name option;
  down : index_name option;
  show : show list;
}

(* Note that down and default_index can't actually happen on Root_root *)
let rec root =
  lazy
    ( "Root",
      [
        {
          name = "root";
          args = [];
          indexes = [ ("root", exp) ];
          default = Some "root";
          down = Some "root";
          show = [ Index "root" ];
        };
      ] )

and exp =
  lazy
    ( "Exp",
      [
        {
          name = "var";
          args = [ ("string", "(fun x -> x)") ];
          indexes = [];
          default = None;
          down = None;
          show = [ Arg (0, "(fun x -> x)") ];
        };
        {
          name = "lam";
          args = [];
          indexes = [ ("param", pat); ("param_type", typ); ("body", exp) ];
          default = Some "param";
          down = Some "body";
          show =
            [
              String "(λ";
              Index "param";
              String ":";
              Index "param_type";
              String ".";
              Index "body";
              String ")";
            ];
        };
        {
          name = "app";
          args = [];
          indexes = [ ("fun", exp); ("arg", exp) ];
          default = Some "fun";
          down = Some "fun";
          show = [ Index "fun"; String " "; Index "arg" ];
        };
        {
          name = "num";
          args = [ ("int", "Int.to_string") ];
          indexes = [];
          default = None;
          down = None;
          show = [ Arg (0, "Int.to_string") ];
        };
        {
          name = "plus";
          args = [];
          indexes = [ ("left", exp); ("right", exp) ];
          default = Some "left";
          down = Some "left";
          show = [ Index "left"; String " + "; Index "right" ];
        };
        {
          name = "times";
          args = [];
          indexes = [ ("left", exp); ("right", exp) ];
          default = Some "left";
          down = Some "left";
          show = [ Index "left"; String " * "; Index "right" ];
        };
        (* TODO: sums and pairs *)
        {
          name = "nil";
          args = [];
          indexes = [];
          default = None;
          down = None;
          show = [ String "Int" ];
        };
        {
          name = "cons";
          args = [];
          indexes = [ ("head", exp); ("tail", exp) ];
          default = Some "head";
          down = Some "head";
          show = [ Index "head"; String "::"; Index "tail" ];
        };
        {
          name = "case";
          args = [];
          indexes =
            [
              ("scrut", exp);
              ("nil_body", exp);
              ("head_pat", pat);
              ("tail_pat", pat);
              ("cons_body", exp);
              ("type", typ);
            ];
          default = Some "scrut";
          down = Some "scrut";
          show =
            [
              String "case";
              Index "scrut";
              String "of";
              String "nil";
              String "->";
              Index "nil_body";
              String "|";
              Index "head_pat";
              String "::";
              Index "tail_pat";
              String "->";
              Index "cons_body";
              String "end";
              String ":";
              Index "type";
            ];
        };
      ] )

and pat =
  lazy
    ( "Pat",
      [
        {
          name = "var";
          args = [ ("string", "(fun x -> x)") ];
          indexes = [];
          default = None;
          down = None;
          show = [ Arg (0, "(fun x -> x)") ];
        };
      ] )

and typ =
  lazy
    ( "Typ",
      [
        {
          name = "num";
          args = [];
          indexes = [];
          default = None;
          down = None;
          show = [ String "Num" ];
        };
        {
          name = "arrow";
          args = [];
          indexes = [ ("arg", typ); ("result", typ) ];
          default = Some "arg";
          down = Some "arg";
          show = [ Index "arg"; String "→"; Index "result" ];
        };
        {
          name = "list";
          args = [];
          indexes = [ ("content", typ) ];
          default = Some "content";
          down = Some "content";
          show = [ String "list"; String "["; Index "content"; String "]" ];
        };
      ] )

let sorts = [ root; exp; pat; typ ]

(* TODO: pretty print data *)

(**** Helpers for mode ideomatic code ****)
let f : ('a, unit, string) format -> 'a = Printf.sprintf

let cat ?(sep : string option) ?(empty : string option) (f : 'a -> string)
    (l : 'a list) : string =
  let sep = Option.value ~default:"" sep in
  match (empty, l) with
  | Some empty, [] -> empty
  | _ -> String.concat sep (List.map f l)

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
    let mk_arg (t, _) : string = t in
    let mk_constructor s c : string =
      let args =
        match c.args with
        | [] -> ""
        | _ -> f " of (%s)" (cat ~sep:", " mk_arg c.args)
      in
      f "\n    | %s_%s%s" s c.name args
    in
    let mk_sort ((lazy (s, cs)) : sort) : string =
      f "\n    (**** %s ****)%s" s (cat (mk_constructor s) cs)
    in
    cat mk_sort sorts
  in
  (**** Function body for `sort_of` ****)
  let sort_of : string =
    let mk_constructor s c : string =
      f "\n    | %s_%s%s -> %s" s c.name (arg_pat c.args) s
    in
    let mk_sort ((lazy (s, cs)) : sort) : string =
      f "\n    (**** %s ****)%s" s (cat (mk_constructor s) cs)
    in
    cat mk_sort sorts
  in
  (**** Function body for `graphviz_label` ****)
  let graphviz_label : string =
    let mk_constructor s c : string =
      let args =
        match c.args with
        | [] -> ""
        | _ ->
            f "(%s)"
              (String.concat ", "
                 (List.mapi
                    (fun i (_, graphviz_string_of) ->
                      f "\" ^ %s arg%d ^ \"" graphviz_string_of i)
                    c.args))
      in
      f "\n    | %s_%s%s -> \"%s%s\"" s c.name (bindings c.args) c.name args
    in
    let mk_sort ((lazy (s, cs)) : sort) : string =
      f "\n    (**** %s ****)%s" s (cat (mk_constructor s) cs)
    in
    cat mk_sort sorts
  in
  (**** Module declaration ****)
  Printf.printf
    {|
module Constructor = struct
  open Sexplib0.Sexp_conv

  type t =%s
  [@@deriving show, eq, ord, sexp]

  (* Returns the sort of a particular constructor *)
  let sort_of (c : t) : Sort.t =
    match c with%s

  (* Specifies the label to use in graphviz *)
  let graphviz_label (c : t) : string =
    match c with%s
end
|}
    t sort_of graphviz_label

(* TODO: rename Index to Field, position, Child_index, child position, child slot? *)
(**** Module: Index ****)
let () =
  (**** Type declaration ****)
  let t =
    let mk_index s c ((i, _) : index) : string = f "\n    | %s_%s_%s" s c i in
    let mk_constructor s c : string =
      f "\n    (* %s_%s *)%s" s c.name
        (cat ~empty (mk_index s c.name) c.indexes)
    in
    let mk_sort (lazy (s, cs)) : string =
      f "\n    (**** %s ****)%s" s (cat (mk_constructor s) cs)
    in
    cat mk_sort sorts
  in
  (**** Function body for `short_name` ****)
  let short_name : string =
    let mk_index s c ((i, _) : index) : string =
      f "\n    | %s_%s_%s -> \"%s\"" s c i i
    in
    let mk_constructor s c : string =
      f "\n    (* %s_%s *)%s" s c.name
        (cat ~empty (mk_index s c.name) c.indexes)
    in
    let mk_sort ((lazy (s, cs)) : sort) : string =
      f "\n    (**** %s ****)%s" s (cat (mk_constructor s) cs)
    in
    cat mk_sort sorts
  in
  (**** Function body for `parent_constructor` ****)
  let parent_constructor : string =
    let mk_index s c ((i, _) : index) : string =
      f "\n    | %s_%s_%s -> %s_%s" s c i s c
    in
    let mk_constructor s c : string =
      f "\n    (* %s_%s *)%s" s c.name
        (cat ~empty (mk_index s c.name) c.indexes)
    in
    let mk_sort ((lazy (s, cs)) : sort) : string =
      f "\n    (**** %s ****)%s" s (cat (mk_constructor s) cs)
    in
    cat mk_sort sorts
  in
  (**** Function body for `child_indexes` ****)
  let child_indexes : string =
    let mk_index s c ((i, _) : index) : string = f "%s_%s_%s" s c i in
    let mk_constructor s c : string =
      f "\n    | %s_%s%s -> [%s]" s c.name (arg_pat c.args)
        (cat ~sep:"; " (mk_index s c.name) c.indexes)
    in
    let mk_sort ((lazy (s, cs)) : sort) : string =
      f "\n    (**** %s ****)%s" s (cat (mk_constructor s) cs)
    in
    cat mk_sort sorts
  in
  (**** Function body for `child_sort` ****)
  let child_sort : string =
    let mk_index s c ((i, (lazy (s_i, _))) : index) : string =
      f "\n    | %s_%s_%s -> %s" s c i s_i
    in
    let mk_constructor s c : string =
      f "\n    (* %s_%s *)%s" s c.name
        (cat ~empty (mk_index s c.name) c.indexes)
    in
    let mk_sort ((lazy (s, cs)) : sort) : string =
      f "\n    (**** %s ****)%s" s (cat (mk_constructor s) cs)
    in
    cat mk_sort sorts
  in
  (**** Function body for `default_index` ****)
  let default_index : string =
    let mk_constructor s c : string =
      f "\n    | %s_%s%s -> %s" s c.name (arg_pat c.args)
        ( match c.default with
        | None -> "None"
        | Some d -> f "Some %s_%s_%s" s c.name d )
    in
    let mk_sort ((lazy (s, cs)) : sort) : string =
      f "\n    (**** %s ****)%s" s (cat (mk_constructor s) cs)
    in
    cat mk_sort sorts
  in
  (**** Function body for `down` ****)
  let down : string =
    let mk_constructor s c : string =
      f "\n    | %s_%s%s -> %s" s c.name (arg_pat c.args)
        ( match c.down with
        | None -> "None"
        | Some d -> f "Some %s_%s_%s" s c.name d )
    in
    let mk_sort ((lazy (s, cs)) : sort) : string =
      f "\n    (**** %s ****)%s" s (cat (mk_constructor s) cs)
    in
    cat mk_sort sorts
  in
  (**** Function body for `right` ****)
  let right : string =
    let mk_index s c (((i1, _) : index), i2) : string =
      match i2 with
      | None -> f "\n    | %s_%s_%s -> None" s c i1
      | Some (i2, _) -> f "\n    | %s_%s_%s -> Some %s_%s_%s" s c i1 s c i2
    in
    let mk_constructor s c : string =
      f "\n    (* %s_%s *)%s" s c.name
        (cat ~empty (mk_index s c.name) (shift_forward c.indexes))
    in
    let mk_sort ((lazy (s, cs)) : sort) : string =
      f "\n    (**** %s ****)%s" s (cat (mk_constructor s) cs)
    in
    cat mk_sort sorts
  in
  (**** Function body for `left` ****)
  let left : string =
    let mk_index s c (((i1, _) : index), i2) : string =
      match i2 with
      | None -> f "\n    | %s_%s_%s -> None" s c i1
      | Some (i2, _) -> f "\n    | %s_%s_%s -> Some %s_%s_%s" s c i1 s c i2
    in
    let mk_constructor s c : string =
      f "\n    (* %s_%s *)%s" s c.name
        (cat ~empty (mk_index s c.name) (shift_backward c.indexes))
    in
    let mk_sort ((lazy (s, cs)) : sort) : string =
      f "\n    (**** %s ****)%s" s (cat (mk_constructor s) cs)
    in
    cat mk_sort sorts
  in
  (**** Module declaration ****)
  Printf.printf
    {|
module Index = struct
  type t =%s
  [@@deriving show, eq, ord, sexp]

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
end
|}
    t short_name parent_constructor child_indexes child_sort default_index down
    right left

(**** Module: Gadt ****)
let () =
  (**** Type declaration ****)
  let types =
    let mk_constructor s c : string =
      f "\n  type %s_%s = %s_%s (*of %%s*) [@@deriving show, eq, ord, sexp_of]"
        (String.lowercase_ascii s) c.name s c.name
      (* TODO: indexes *)
    in
    let mk_sort (lazy (s, cs)) : string =
      f "\n  type %s%s" (String.lowercase_ascii s) (cat (mk_constructor s) cs)
    in
    cat mk_sort sorts
  in
  (**** Function body for `sort_of_ctor` ****)
  (**** Function body for `sort_of_child` ****)
  (**** Function body for `short_name` ****)
  let short_name : string =
    let mk_index s c ((i, _) : index) : string =
      f "\n    | %s_%s_%s -> \"%s\"" s c i i
    in
    let mk_constructor s c : string =
      f "\n    (* %s_%s *)%s" s c.name
        (cat ~empty (mk_index s c.name) c.indexes)
    in
    let mk_sort ((lazy (s, cs)) : sort) : string =
      f "\n    (**** %s ****)%s" s (cat (mk_constructor s) cs)
    in
    cat mk_sort sorts
  in
  (**** Module declaration ****)
  Printf.printf
    {|
module Gadt = struct
  %s

(*type ('ctor, 'sort) sort_of_ctor =*)

(*
  let short_name (i : t) : string =
    match i with%s
    *)
end
|}
    types short_name

(**** Module: <top-level> ****)
let () =
  (**** Function body for `show` ****)
  let show : string =
    let mk_show s c (show : show) : string =
      match show with
      | String s -> f "string %S" s
      | Arg (i, s) -> f "arg (%s arg%d)" s i
      | Index i -> f "index %s_%s_%s" s c i
    in
    let mk_constructor s c : string =
      f "\n    | %s_%s%s -> [%s]" s c.name (bindings c.args)
        (cat ~sep:"; " (mk_show s c.name) c.show)
    in
    let mk_sort ((lazy (s, cs)) : sort) : string =
      f "\n    (**** %s ****)%s" s (cat (mk_constructor s) cs)
    in
    cat mk_sort sorts
  in
  (**** Module declaration ****)
  Printf.printf
    {|
(* Specifies how to show the given constructor and its children *)
let show (string : string -> 'a) (arg : string -> 'a) (index : Index.t -> 'a) (c : Constructor.t) : 'a list =
  match c with%s
|}
    show
