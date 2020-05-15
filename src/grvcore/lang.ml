module Sort = struct
  type t = Root | Id | Exp | Typ
end

module Constructor = struct
  open Sexplib0.Sexp_conv

  type t =
    (**** Root ****)
    | Root_root
    (**** Id ****)
    | Id_id of string
    (**** Exp ****)
    | Exp_var
    | Exp_lam
    | Exp_app
    | Exp_num of int
    | Exp_plus
    (* TODO: sums and pairs *)
    (**** Typ ****)
    | Typ_num
    | Typ_arrow
  [@@deriving show, sexp_of]
end

module Index = struct
  (* TODO: rename to "field", "child_index"? *)

  type t =
    (**** Root ****)
    (* Root_root *)
    | Root_root_root
    (**** Id ****)
    (* Id_id *)
    (* -- EMPTY -- *)
    (**** Exp ****)
    (* Exp_var *)
    | Exp_var_id
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
    (**** Typ ****)
    (* Typ_num *)
    (* -- EMPTY -- *)
    (* Typ_arrow *)
    | Typ_arrow_arg
    | Typ_arrow_result
  [@@deriving show, compare, sexp_of]

  (* TODO: programatically generate these *)

  (* Specifies where the existing node goes when a node is wrapped by the given constructor *)
  let default_index (ctor : Constructor.t) : t option =
    match ctor with
    | Root_root -> (* NOTE: Can't actually happen *) Some Root_root_root
    | Id_id _ -> None
    | Exp_var -> Some Exp_var_id
    | Exp_lam -> Some Exp_lam_param
    | Exp_app -> Some Exp_app_fun
    | Exp_num _ -> None
    | Exp_plus -> Some Exp_plus_left
    | Typ_num -> None
    | Typ_arrow -> Some Typ_arrow_arg

  (* Specifies where to go when the cursor moves down *)
  let down (ctor : Constructor.t) : t option =
    match ctor with
    | Root_root -> (* NOTE: Can't actually happen *) Some Root_root_root
    | Id_id _ -> None
    | Exp_var -> Some Exp_var_id
    | Exp_lam -> Some Exp_lam_param
    | Exp_app -> Some Exp_app_fun
    | Exp_num _ -> None
    | Exp_plus -> Some Exp_plus_left
    | Typ_num -> None
    | Typ_arrow -> Some Typ_arrow_arg

  let right (index : t) : t option =
    match index with
    | Root_root_root -> None
    | Exp_var_id -> None
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
    | Exp_var_id -> None
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
