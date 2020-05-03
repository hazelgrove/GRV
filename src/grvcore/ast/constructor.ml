type t =
  | Root_root
  | Id_id of string
  | Exp_lam
  | Exp_app
  | Exp_var
  | Typ_app
  | Typ_var
[@@deriving show]
