
module Sort = struct
  type t = 
    | Root
    | Exp
    | Pat
    | Typ
  [@@deriving show, eq, ord, sexp_of]
end

module Constructor = struct
  open Sexplib0.Sexp_conv

  type t =
    (**** Root ****)
    | Root_root
    (**** Exp ****)
    | Exp_var of (string)
    | Exp_lam
    | Exp_app
    | Exp_num of (int)
    | Exp_plus
    | Exp_times
    | Exp_nil
    | Exp_cons
    | Exp_case
    (**** Pat ****)
    | Pat_var of (string)
    (**** Typ ****)
    | Typ_num
    | Typ_arrow
    | Typ_list
  [@@deriving show, eq, ord, sexp]

  (* Returns the sort of a particular constructor *)
  let sort_of (c : t) : Sort.t =
    match c with
    (**** Root ****)
    | Root_root -> Root
    (**** Exp ****)
    | Exp_var _ -> Exp
    | Exp_lam -> Exp
    | Exp_app -> Exp
    | Exp_num _ -> Exp
    | Exp_plus -> Exp
    | Exp_times -> Exp
    | Exp_nil -> Exp
    | Exp_cons -> Exp
    | Exp_case -> Exp
    (**** Pat ****)
    | Pat_var _ -> Pat
    (**** Typ ****)
    | Typ_num -> Typ
    | Typ_arrow -> Typ
    | Typ_list -> Typ

  (* Specifies the label to use in graphviz *)
  let graphviz_label (c : t) : string =
    match c with
    (**** Root ****)
    | Root_root -> "root"
    (**** Exp ****)
    | Exp_var (arg0) -> "var(" ^ (fun x -> x) arg0 ^ ")"
    | Exp_lam -> "lam"
    | Exp_app -> "app"
    | Exp_num (arg0) -> "num(" ^ Int.to_string arg0 ^ ")"
    | Exp_plus -> "plus"
    | Exp_times -> "times"
    | Exp_nil -> "nil"
    | Exp_cons -> "cons"
    | Exp_case -> "case"
    (**** Pat ****)
    | Pat_var (arg0) -> "var(" ^ (fun x -> x) arg0 ^ ")"
    (**** Typ ****)
    | Typ_num -> "num"
    | Typ_arrow -> "arrow"
    | Typ_list -> "list"
end

module Position = struct
  type t =
    (**** Root ****)
    (* Root_root *)
    | Root_root_root
    (**** Exp ****)
    (* Exp_var *)
    (* -- empty -- *)
    (* Exp_lam *)
    | Exp_lam_param
    | Exp_lam_param_type
    | Exp_lam_body
    (* Exp_app *)
    | Exp_app_fun
    | Exp_app_arg
    (* Exp_num *)
    (* -- empty -- *)
    (* Exp_plus *)
    | Exp_plus_left
    | Exp_plus_right
    (* Exp_times *)
    | Exp_times_left
    | Exp_times_right
    (* Exp_nil *)
    (* -- empty -- *)
    (* Exp_cons *)
    | Exp_cons_head
    | Exp_cons_tail
    (* Exp_case *)
    | Exp_case_scrut
    | Exp_case_nil_body
    | Exp_case_head_pat
    | Exp_case_tail_pat
    | Exp_case_cons_body
    | Exp_case_type
    (**** Pat ****)
    (* Pat_var *)
    (* -- empty -- *)
    (**** Typ ****)
    (* Typ_num *)
    (* -- empty -- *)
    (* Typ_arrow *)
    | Typ_arrow_arg
    | Typ_arrow_result
    (* Typ_list *)
    | Typ_list_content
  [@@deriving show, eq, ord, sexp]

  let short_name (i : t) : string =
    match i with
    (**** Root ****)
    (* Root_root *)
    | Root_root_root -> "root"
    (**** Exp ****)
    (* Exp_var *)
    (* -- empty -- *)
    (* Exp_lam *)
    | Exp_lam_param -> "param"
    | Exp_lam_param_type -> "param_type"
    | Exp_lam_body -> "body"
    (* Exp_app *)
    | Exp_app_fun -> "fun"
    | Exp_app_arg -> "arg"
    (* Exp_num *)
    (* -- empty -- *)
    (* Exp_plus *)
    | Exp_plus_left -> "left"
    | Exp_plus_right -> "right"
    (* Exp_times *)
    | Exp_times_left -> "left"
    | Exp_times_right -> "right"
    (* Exp_nil *)
    (* -- empty -- *)
    (* Exp_cons *)
    | Exp_cons_head -> "head"
    | Exp_cons_tail -> "tail"
    (* Exp_case *)
    | Exp_case_scrut -> "scrut"
    | Exp_case_nil_body -> "nil_body"
    | Exp_case_head_pat -> "head_pat"
    | Exp_case_tail_pat -> "tail_pat"
    | Exp_case_cons_body -> "cons_body"
    | Exp_case_type -> "type"
    (**** Pat ****)
    (* Pat_var *)
    (* -- empty -- *)
    (**** Typ ****)
    (* Typ_num *)
    (* -- empty -- *)
    (* Typ_arrow *)
    | Typ_arrow_arg -> "arg"
    | Typ_arrow_result -> "result"
    (* Typ_list *)
    | Typ_list_content -> "content"

  (* Returns the constructor that parents of a particular position should have *)
  let parent_constructor (i : t) : Constructor.t =
    match i with
    (**** Root ****)
    (* Root_root *)
    | Root_root_root -> Root_root
    (**** Exp ****)
    (* Exp_var *)
    (* -- empty -- *)
    (* Exp_lam *)
    | Exp_lam_param -> Exp_lam
    | Exp_lam_param_type -> Exp_lam
    | Exp_lam_body -> Exp_lam
    (* Exp_app *)
    | Exp_app_fun -> Exp_app
    | Exp_app_arg -> Exp_app
    (* Exp_num *)
    (* -- empty -- *)
    (* Exp_plus *)
    | Exp_plus_left -> Exp_plus
    | Exp_plus_right -> Exp_plus
    (* Exp_times *)
    | Exp_times_left -> Exp_times
    | Exp_times_right -> Exp_times
    (* Exp_nil *)
    (* -- empty -- *)
    (* Exp_cons *)
    | Exp_cons_head -> Exp_cons
    | Exp_cons_tail -> Exp_cons
    (* Exp_case *)
    | Exp_case_scrut -> Exp_case
    | Exp_case_nil_body -> Exp_case
    | Exp_case_head_pat -> Exp_case
    | Exp_case_tail_pat -> Exp_case
    | Exp_case_cons_body -> Exp_case
    | Exp_case_type -> Exp_case
    (**** Pat ****)
    (* Pat_var *)
    (* -- empty -- *)
    (**** Typ ****)
    (* Typ_num *)
    (* -- empty -- *)
    (* Typ_arrow *)
    | Typ_arrow_arg -> Typ_arrow
    | Typ_arrow_result -> Typ_arrow
    (* Typ_list *)
    | Typ_list_content -> Typ_list

  (* Returns the constructor that parents of a particular position should have *)
  let child_positions (c : Constructor.t) : t list =
    match c with
    (**** Root ****)
    | Root_root -> [Root_root_root]
    (**** Exp ****)
    | Exp_var _ -> []
    | Exp_lam -> [Exp_lam_param; Exp_lam_param_type; Exp_lam_body]
    | Exp_app -> [Exp_app_fun; Exp_app_arg]
    | Exp_num _ -> []
    | Exp_plus -> [Exp_plus_left; Exp_plus_right]
    | Exp_times -> [Exp_times_left; Exp_times_right]
    | Exp_nil -> []
    | Exp_cons -> [Exp_cons_head; Exp_cons_tail]
    | Exp_case -> [Exp_case_scrut; Exp_case_nil_body; Exp_case_head_pat; Exp_case_tail_pat; Exp_case_cons_body; Exp_case_type]
    (**** Pat ****)
    | Pat_var _ -> []
    (**** Typ ****)
    | Typ_num -> []
    | Typ_arrow -> [Typ_arrow_arg; Typ_arrow_result]
    | Typ_list -> [Typ_list_content]

  (* Returns the sort that children of a particular position should have *)
  let child_sort (i : t) : Sort.t =
    match i with
    (**** Root ****)
    (* Root_root *)
    | Root_root_root -> Exp
    (**** Exp ****)
    (* Exp_var *)
    (* -- empty -- *)
    (* Exp_lam *)
    | Exp_lam_param -> Pat
    | Exp_lam_param_type -> Typ
    | Exp_lam_body -> Exp
    (* Exp_app *)
    | Exp_app_fun -> Exp
    | Exp_app_arg -> Exp
    (* Exp_num *)
    (* -- empty -- *)
    (* Exp_plus *)
    | Exp_plus_left -> Exp
    | Exp_plus_right -> Exp
    (* Exp_times *)
    | Exp_times_left -> Exp
    | Exp_times_right -> Exp
    (* Exp_nil *)
    (* -- empty -- *)
    (* Exp_cons *)
    | Exp_cons_head -> Exp
    | Exp_cons_tail -> Exp
    (* Exp_case *)
    | Exp_case_scrut -> Exp
    | Exp_case_nil_body -> Exp
    | Exp_case_head_pat -> Pat
    | Exp_case_tail_pat -> Pat
    | Exp_case_cons_body -> Exp
    | Exp_case_type -> Typ
    (**** Pat ****)
    (* Pat_var *)
    (* -- empty -- *)
    (**** Typ ****)
    (* Typ_num *)
    (* -- empty -- *)
    (* Typ_arrow *)
    | Typ_arrow_arg -> Typ
    | Typ_arrow_result -> Typ
    (* Typ_list *)
    | Typ_list_content -> Typ

  (* Specifies where the existing node goes when a node is wrapped by the given constructor *)
  let default_position (c : Constructor.t) : t option =
    match c with
    (**** Root ****)
    | Root_root -> Some Root_root_root
    (**** Exp ****)
    | Exp_var _ -> None
    | Exp_lam -> Some Exp_lam_param
    | Exp_app -> Some Exp_app_fun
    | Exp_num _ -> None
    | Exp_plus -> Some Exp_plus_left
    | Exp_times -> Some Exp_times_left
    | Exp_nil -> None
    | Exp_cons -> Some Exp_cons_head
    | Exp_case -> Some Exp_case_scrut
    (**** Pat ****)
    | Pat_var _ -> None
    (**** Typ ****)
    | Typ_num -> None
    | Typ_arrow -> Some Typ_arrow_arg
    | Typ_list -> Some Typ_list_content

  (* Specifies where to go when the cursor moves down *)
  let down (c : Constructor.t) : t option =
    match c with
    (**** Root ****)
    | Root_root -> Some Root_root_root
    (**** Exp ****)
    | Exp_var _ -> None
    | Exp_lam -> Some Exp_lam_body
    | Exp_app -> Some Exp_app_fun
    | Exp_num _ -> None
    | Exp_plus -> Some Exp_plus_left
    | Exp_times -> Some Exp_times_left
    | Exp_nil -> None
    | Exp_cons -> Some Exp_cons_head
    | Exp_case -> Some Exp_case_scrut
    (**** Pat ****)
    | Pat_var _ -> None
    (**** Typ ****)
    | Typ_num -> None
    | Typ_arrow -> Some Typ_arrow_arg
    | Typ_list -> Some Typ_list_content

  (* Specifies where to go when the cursor moves right *)
  let right (i : t) : t option =
    match i with
    (**** Root ****)
    (* Root_root *)
    | Root_root_root -> None
    (**** Exp ****)
    (* Exp_var *)
    (* -- empty -- *)
    (* Exp_lam *)
    | Exp_lam_param -> Some Exp_lam_param_type
    | Exp_lam_param_type -> Some Exp_lam_body
    | Exp_lam_body -> None
    (* Exp_app *)
    | Exp_app_fun -> Some Exp_app_arg
    | Exp_app_arg -> None
    (* Exp_num *)
    (* -- empty -- *)
    (* Exp_plus *)
    | Exp_plus_left -> Some Exp_plus_right
    | Exp_plus_right -> None
    (* Exp_times *)
    | Exp_times_left -> Some Exp_times_right
    | Exp_times_right -> None
    (* Exp_nil *)
    (* -- empty -- *)
    (* Exp_cons *)
    | Exp_cons_head -> Some Exp_cons_tail
    | Exp_cons_tail -> None
    (* Exp_case *)
    | Exp_case_scrut -> Some Exp_case_nil_body
    | Exp_case_nil_body -> Some Exp_case_head_pat
    | Exp_case_head_pat -> Some Exp_case_tail_pat
    | Exp_case_tail_pat -> Some Exp_case_cons_body
    | Exp_case_cons_body -> Some Exp_case_type
    | Exp_case_type -> None
    (**** Pat ****)
    (* Pat_var *)
    (* -- empty -- *)
    (**** Typ ****)
    (* Typ_num *)
    (* -- empty -- *)
    (* Typ_arrow *)
    | Typ_arrow_arg -> Some Typ_arrow_result
    | Typ_arrow_result -> None
    (* Typ_list *)
    | Typ_list_content -> None

  (* Specifies where to go when the cursor moves left *)
  let left (i : t) : t option =
    match i with
    (**** Root ****)
    (* Root_root *)
    | Root_root_root -> None
    (**** Exp ****)
    (* Exp_var *)
    (* -- empty -- *)
    (* Exp_lam *)
    | Exp_lam_param -> None
    | Exp_lam_param_type -> Some Exp_lam_param
    | Exp_lam_body -> Some Exp_lam_param_type
    (* Exp_app *)
    | Exp_app_fun -> None
    | Exp_app_arg -> Some Exp_app_fun
    (* Exp_num *)
    (* -- empty -- *)
    (* Exp_plus *)
    | Exp_plus_left -> None
    | Exp_plus_right -> Some Exp_plus_left
    (* Exp_times *)
    | Exp_times_left -> None
    | Exp_times_right -> Some Exp_times_left
    (* Exp_nil *)
    (* -- empty -- *)
    (* Exp_cons *)
    | Exp_cons_head -> None
    | Exp_cons_tail -> Some Exp_cons_head
    (* Exp_case *)
    | Exp_case_scrut -> None
    | Exp_case_nil_body -> Some Exp_case_scrut
    | Exp_case_head_pat -> Some Exp_case_nil_body
    | Exp_case_tail_pat -> Some Exp_case_head_pat
    | Exp_case_cons_body -> Some Exp_case_tail_pat
    | Exp_case_type -> Some Exp_case_cons_body
    (**** Pat ****)
    (* Pat_var *)
    (* -- empty -- *)
    (**** Typ ****)
    (* Typ_num *)
    (* -- empty -- *)
    (* Typ_arrow *)
    | Typ_arrow_arg -> None
    | Typ_arrow_result -> Some Typ_arrow_arg
    (* Typ_list *)
    | Typ_list_content -> None
end

module Gadt = struct
  
  type root
  type root_root = Root_root (*of %s*) [@@deriving show, eq, ord, sexp_of]
  type exp
  type exp_var = Exp_var (*of %s*) [@@deriving show, eq, ord, sexp_of]
  type exp_lam = Exp_lam (*of %s*) [@@deriving show, eq, ord, sexp_of]
  type exp_app = Exp_app (*of %s*) [@@deriving show, eq, ord, sexp_of]
  type exp_num = Exp_num (*of %s*) [@@deriving show, eq, ord, sexp_of]
  type exp_plus = Exp_plus (*of %s*) [@@deriving show, eq, ord, sexp_of]
  type exp_times = Exp_times (*of %s*) [@@deriving show, eq, ord, sexp_of]
  type exp_nil = Exp_nil (*of %s*) [@@deriving show, eq, ord, sexp_of]
  type exp_cons = Exp_cons (*of %s*) [@@deriving show, eq, ord, sexp_of]
  type exp_case = Exp_case (*of %s*) [@@deriving show, eq, ord, sexp_of]
  type pat
  type pat_var = Pat_var (*of %s*) [@@deriving show, eq, ord, sexp_of]
  type typ
  type typ_num = Typ_num (*of %s*) [@@deriving show, eq, ord, sexp_of]
  type typ_arrow = Typ_arrow (*of %s*) [@@deriving show, eq, ord, sexp_of]
  type typ_list = Typ_list (*of %s*) [@@deriving show, eq, ord, sexp_of]

(*type ('ctor, 'sort) sort_of_ctor =*)

(*
  let short_name (i : t) : string =
    match i with
    (**** Root ****)
    (* Root_root *)
    | Root_root_root -> "root"
    (**** Exp ****)
    (* Exp_var *)
    (* -- empty -- *)
    (* Exp_lam *)
    | Exp_lam_param -> "param"
    | Exp_lam_param_type -> "param_type"
    | Exp_lam_body -> "body"
    (* Exp_app *)
    | Exp_app_fun -> "fun"
    | Exp_app_arg -> "arg"
    (* Exp_num *)
    (* -- empty -- *)
    (* Exp_plus *)
    | Exp_plus_left -> "left"
    | Exp_plus_right -> "right"
    (* Exp_times *)
    | Exp_times_left -> "left"
    | Exp_times_right -> "right"
    (* Exp_nil *)
    (* -- empty -- *)
    (* Exp_cons *)
    | Exp_cons_head -> "head"
    | Exp_cons_tail -> "tail"
    (* Exp_case *)
    | Exp_case_scrut -> "scrut"
    | Exp_case_nil_body -> "nil_body"
    | Exp_case_head_pat -> "head_pat"
    | Exp_case_tail_pat -> "tail_pat"
    | Exp_case_cons_body -> "cons_body"
    | Exp_case_type -> "type"
    (**** Pat ****)
    (* Pat_var *)
    (* -- empty -- *)
    (**** Typ ****)
    (* Typ_num *)
    (* -- empty -- *)
    (* Typ_arrow *)
    | Typ_arrow_arg -> "arg"
    | Typ_arrow_result -> "result"
    (* Typ_list *)
    | Typ_list_content -> "content"
    *)
end

(* Specifies how to show the given constructor and its children *)
let show (string : string -> 'a) (arg : string -> 'a)
    (position : Position.t -> 'a) (c : Constructor.t) : 'a list =
  match c with
    (**** Root ****)
    | Root_root -> [position Root_root_root]
    (**** Exp ****)
    | Exp_var (arg0) -> [arg ((fun x -> x) arg0)]
    | Exp_lam -> [string "\206\187"; position Exp_lam_param; string ":"; position Exp_lam_param_type; string "."; position Exp_lam_body]
    | Exp_app -> [position Exp_app_fun; string " "; position Exp_app_arg]
    | Exp_num (arg0) -> [arg (Int.to_string arg0)]
    | Exp_plus -> [position Exp_plus_left; string " + "; position Exp_plus_right]
    | Exp_times -> [position Exp_times_left; string " * "; position Exp_times_right]
    | Exp_nil -> [string "Int"]
    | Exp_cons -> [position Exp_cons_head; string "::"; position Exp_cons_tail]
    | Exp_case -> [string "case"; position Exp_case_scrut; string "of"; string "nil"; string "->"; position Exp_case_nil_body; string "|"; position Exp_case_head_pat; string "::"; position Exp_case_tail_pat; string "->"; position Exp_case_cons_body; string "end"; string ":"; position Exp_case_type]
    (**** Pat ****)
    | Pat_var (arg0) -> [arg ((fun x -> x) arg0)]
    (**** Typ ****)
    | Typ_num -> [string "Num"]
    | Typ_arrow -> [position Typ_arrow_arg; string "\226\134\146"; position Typ_arrow_result]
    | Typ_list -> [string "list"; string "["; position Typ_list_content; string "]"]
