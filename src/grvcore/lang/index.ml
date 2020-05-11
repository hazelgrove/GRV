(* TODO: rename to "field", "child_index"? *)

type t =
  | Root_root_root
  | Exp_lam_param
  | Exp_lam_param_type
  | Exp_lam_body
  | Exp_app_fun
  | Exp_app_arg
  | Exp_var_id
  | Typ_app_fun
  | Typ_app_arg
  | Typ_var_id
[@@deriving compare, show]

(* TODO: programatically generate these *)

(* Specifies where the existing node goes when a node is wrapped by the given constructor *)
(* TODO: rename *)
let wrap (ctor : Constructor.t) : t option =
  match ctor with
  | Root_root -> (* NOTE: Can't actually happen *) Some Root_root_root
  | Id_id _ -> None
  | Exp_lam -> Some Exp_lam_param
  | Exp_app -> Some Exp_app_fun
  | Exp_var -> Some Exp_var_id
  | Typ_app -> Some Typ_app_fun
  | Typ_var -> Some Typ_var_id

(* Specifies where to go when the cursor moves down *)
let down (ctor : Constructor.t) : t option =
  match ctor with
  | Root_root -> (* NOTE: Can't actually happen *) Some Root_root_root
  | Id_id _ -> None
  | Exp_lam -> Some Exp_lam_param
  | Exp_app -> Some Exp_app_fun
  | Exp_var -> Some Exp_var_id
  | Typ_app -> Some Typ_app_fun
  | Typ_var -> Some Typ_var_id

let right (index : t) : t option =
  match index with
  | Root_root_root -> None
  | Exp_lam_param -> Some Exp_lam_param_type
  | Exp_lam_param_type -> Some Exp_lam_body
  | Exp_lam_body -> None
  | Exp_app_fun -> Some Exp_app_arg
  | Exp_app_arg -> None
  | Exp_var_id -> None
  | Typ_app_fun -> Some Typ_app_arg
  | Typ_app_arg -> None
  | Typ_var_id -> None

let left (index : t) : t option =
  match index with
  | Root_root_root -> None
  | Exp_lam_param -> None
  | Exp_lam_param_type -> Some Exp_lam_param
  | Exp_lam_body -> Some Exp_lam_param_type
  | Exp_app_fun -> None
  | Exp_app_arg -> Some Exp_app_fun
  | Exp_var_id -> None
  | Typ_app_fun -> None
  | Typ_app_arg -> Some Typ_app_fun
  | Typ_var_id -> None
