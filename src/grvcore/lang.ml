module Sort = struct
  type t = Root | Id | Exp | Pat | Typ
end

module Constructor = struct
  open Sexplib0.Sexp_conv

  type t =
    (**** Root ****)
    | Root_root
    (**** Exp ****)
    | Exp_var of string
    | Exp_lam
    | Exp_app
    | Exp_num of int
    | Exp_plus
    (* TODO: sums and pairs *)
    (**** Pat ****)
    | Pat_var of string
    (**** Typ ****)
    | Typ_num
    | Typ_arrow
  [@@deriving show, sexp_of]

  let sort_of (c : t) : Sort.t =
    match c with
    | Root_root -> Sort.Root
    | Exp_var _ -> Sort.Exp
    | Exp_lam -> Sort.Exp
    | Exp_app -> Sort.Exp
    | Exp_num _ -> Sort.Exp
    | Exp_plus -> Sort.Exp
    | Pat_var _ -> Sort.Exp
    | Typ_num -> Sort.Typ
    | Typ_arrow -> Sort.Typ
end

module Index = struct
  (* TODO: rename to "field", "child_index"? *)

  type t =
    (**** Root ****)
    (* Root_root *)
    | Root_root_root
    (**** Exp ****)
    (* Exp_var *)
    (* -- EMPTY -- *)
    (* Exp_lam *)
    | Exp_lam_param
    | Exp_lam_param_type
    | Exp_lam_body
    (* Exp_app *)
    | Exp_app_fun
    | Exp_app_arg
    (* Exp_num *)
    (* -- EMPTY -- *)
    (* Exp_plus *)
    | Exp_plus_left
    | Exp_plus_right
    (**** Pat ****)
    (* Pat_var *)
    (* -- EMPTY -- *)
    (**** Typ ****)
    (* Typ_num *)
    (* -- EMPTY -- *)
    (* Typ_arrow *)
    | Typ_arrow_arg
    | Typ_arrow_result
  [@@deriving show, compare, sexp_of]

  let child_sort (i : t) : Sort.t =
    match i with
    | Root_root_root -> Sort.Exp
    | Exp_lam_param -> Sort.Id
    | Exp_lam_param_type -> Sort.Typ
    | Exp_lam_body -> Sort.Exp
    | Exp_app_fun -> Sort.Exp
    | Exp_app_arg -> Sort.Exp
    | Exp_plus_left -> Sort.Exp
    | Exp_plus_right -> Sort.Exp
    | Typ_arrow_arg -> Sort.Typ
    | Typ_arrow_result -> Sort.Typ

  (* TODO: programatically generate these *)

  (* Specifies where the existing node goes when a node is wrapped by the given constructor *)
  let default_index (ctor : Constructor.t) : t option =
    match ctor with
    | Root_root -> (* NOTE: Can't actually happen *) Some Root_root_root
    | Exp_var _ -> None
    | Exp_lam -> Some Exp_lam_param
    | Exp_app -> Some Exp_app_fun
    | Exp_num _ -> None
    | Exp_plus -> Some Exp_plus_left
    | Pat_var _ -> None
    | Typ_num -> None
    | Typ_arrow -> Some Typ_arrow_arg

  (* Specifies where to go when the cursor moves down *)
  let down (ctor : Constructor.t) : t option =
    match ctor with
    | Root_root -> (* NOTE: Can't actually happen *) Some Root_root_root
    | Exp_var _ -> None
    | Exp_lam -> Some Exp_lam_param
    | Exp_app -> Some Exp_app_fun
    | Exp_num _ -> None
    | Exp_plus -> Some Exp_plus_left
    | Pat_var _ -> None
    | Typ_num -> None
    | Typ_arrow -> Some Typ_arrow_arg

  let right (index : t) : t option =
    match index with
    | Root_root_root -> None
    | Exp_lam_param -> Some Exp_lam_param_type
    | Exp_lam_param_type -> Some Exp_lam_body
    | Exp_lam_body -> None
    | Exp_app_fun -> Some Exp_app_arg
    | Exp_app_arg -> None
    | Exp_plus_left -> Some Exp_plus_right
    | Exp_plus_right -> None
    | Typ_arrow_arg -> Some Typ_arrow_result
    | Typ_arrow_result -> None

  let left (index : t) : t option =
    match index with
    | Root_root_root -> None
    | Exp_lam_param -> None
    | Exp_lam_param_type -> Some Exp_lam_param
    | Exp_lam_body -> Some Exp_lam_param_type
    | Exp_app_fun -> None
    | Exp_app_arg -> Some Exp_app_fun
    | Exp_plus_left -> None
    | Exp_plus_right -> Some Exp_plus_left
    | Typ_arrow_arg -> None
    | Typ_arrow_result -> Some Typ_arrow_arg
end
