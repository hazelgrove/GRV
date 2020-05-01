type t =
  | Root_root_root
  | Exp_lam_param
  | Exp_lam_param_type
  | Exp_lam_body
  | Exp_app_fun
  | Exp_app_arg
  | Exp_var_id
  | Typ_app_fun
[@@deriving compare, show]
