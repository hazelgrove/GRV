type sort =
  | Root
  | Exp
  | Typ
  | Pat;

type constructor =
  | Root
  | Exp_var(string)
  | Exp_lam
  | Exp_app
  | Exp_plus
  | Exp_times
  | Exp_num(int)
  | Pat_var(string)
  | Typ_arrow
  | Typ_num;

type position =
  | Root_root
  | Lam_param
  | Lam_type
  | Lam_body
  | App_fun
  | App_arg
  | Plus_left
  | Plus_right
  | Times_left
  | Times_right
  | Arrow_arg
  | Arrow_result;

let sort: constructor => sort =
  fun
  | Root => Root
  | Exp_var(_) => Exp
  | Exp_lam => Exp
  | Exp_app => Exp
  | Exp_plus => Exp
  | Exp_times => Exp
  | Exp_num(_) => Exp
  | Pat_var(_) => Pat
  | Typ_arrow => Typ
  | Typ_num => Typ;

let arity: constructor => list((position, sort)) =
  fun
  | Root => []
  | Exp_var(_) => []
  | Exp_lam => [(Lam_param, Pat), (Lam_type, Typ), (Lam_body, Exp)]
  | Exp_app => [(App_fun, Exp), (App_arg, Exp)]
  | Exp_plus => [(Plus_left, Exp), (Plus_right, Exp)]
  | Exp_times => [(Times_left, Exp), (Times_right, Exp)]
  | Exp_num(_) => []
  | Pat_var(_) => []
  | Typ_arrow => [(Arrow_arg, Typ), (Arrow_result, Typ)]
  | Typ_num => [];

let default_position: constructor => option((position, position)) =
  fun
  | Root => Some((Root_root, Root_root))
  | Exp_var(_) => None
  | Exp_lam => Some((Lam_body, Lam_body))
  | Exp_app => Some((App_fun, App_arg))
  | Exp_plus => Some((Plus_left, Plus_right))
  | Exp_times => Some((Times_left, Times_right))
  | Exp_num(_) => None
  | Pat_var(_) => None
  | Typ_arrow => Some((Arrow_arg, Arrow_result))
  | Typ_num => None;
