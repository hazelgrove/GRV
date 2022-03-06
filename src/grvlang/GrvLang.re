module Sig: Sig.S =
  Sig.Make({
    module Sort = {
      type t =
        | Exp
        | Pat
        | Typ;
    };

    module Constructor = {
      type t =
        | Exp_var(string)
        | Exp_lam
        | Exp_app
        | Exm_num(int)
        | Exp_add
        | Exp_mul
        | Pat_var(string)
        | Typ_arr
        | Typ_num;

      let sort: t => Sort.t =
        fun
        | Exp_var(_)
        | Exp_lam
        | Exp_app
        | Exm_num(_)
        | Exp_add
        | Exp_mul => Exp
        | Pat_var(_) => Pat
        | Typ_arr
        | Typ_num => Typ;
    };

    module Position = {
      [@deriving enum]
      type t =
        | Exp_lam_pat
        | Exp_lam_typ
        | Exp_lam_exp
        | Exp_app_fun
        | Exp_app_arg
        | Exp_add_lhs
        | Exp_add_rhs
        | Exp_mul_lhs
        | Exp_mul_rhs
        | Typ_arr_fun
        | Typ_arr_arg;

      let sort: t => Sort.t =
        fun
        | Exp_lam_exp
        | Exp_app_fun
        | Exp_app_arg
        | Exp_add_lhs
        | Exp_add_rhs
        | Exp_mul_lhs
        | Exp_mul_rhs => Exp
        | Exp_lam_pat => Pat
        | Exp_lam_typ
        | Typ_arr_fun
        | Typ_arr_arg => Typ;

      let source: t => Constructor.t =
        fun
        | Exp_lam_pat
        | Exp_lam_typ
        | Exp_lam_exp => Exp_lam
        | Exp_app_fun
        | Exp_app_arg => Exp_app
        | Exp_add_lhs
        | Exp_add_rhs => Exp_add
        | Exp_mul_lhs
        | Exp_mul_rhs => Exp_mul
        | Typ_arr_fun
        | Typ_arr_arg => Typ_arr;
    };
  });
