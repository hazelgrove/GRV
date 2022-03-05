module Sort = {
  type t =
    | Root
    | Exp
    | Typ
    | Pat;

  let show: t => string =
    fun
    | Root => "Root"
    | Exp => "Exp"
    | Typ => "Typ"
    | Pat => "Pat";
};

module Position = {
  type t =
    | Root_root_root
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

  let show: t => string =
    fun
    | Root_root_root => "Root_root_root"
    | Lam_param => "Lam_param"
    | Lam_type => "Lam_type"
    | Lam_body => "Lam_body"
    | App_fun => "App_fun"
    | App_arg => "App_arg"
    | Plus_left => "Plus_left"
    | Plus_right => "Plus_right"
    | Times_left => "Times_left"
    | Times_right => "Times_right"
    | Arrow_arg => "Arrow_arg"
    | Arrow_result => "Arrow_result";

  let sort: t => Sort.t =
    fun
    | Root_root_root => Root
    | Lam_param
    | Lam_type
    | Lam_body
    | App_fun
    | App_arg
    | Plus_left
    | Plus_right
    | Times_left
    | Times_right => Exp
    | Arrow_arg
    | Arrow_result => Typ;
};

module Constructor = {
  type t =
    | Root_root
    | Exp_var(string)
    | Exp_lam
    | Exp_app
    | Exp_plus
    | Exp_times
    | Exp_num(int)
    | Pat_var(string)
    | Typ_arrow
    | Typ_num;

  let show: t => string =
    fun
    | Root_root => "Root_root"
    | Exp_var(_) => "Exp_var"
    | Exp_lam => "Exp_lam"
    | Exp_app => "Exp_app"
    | Exp_plus => "Exp_plus"
    | Exp_times => "Exp_times"
    | Exp_num(_) => "Exp_num"
    | Pat_var(_) => "Pat_var"
    | Typ_arrow => "Typ_arrow"
    | Typ_num => "Typ_num";

  let sort: t => Sort.t =
    fun
    | Root_root => Root
    | Exp_var(_)
    | Exp_lam
    | Exp_app
    | Exp_plus
    | Exp_times
    | Exp_num(_) => Exp
    | Pat_var(_) => Pat
    | Typ_arrow
    | Typ_num => Typ;

  let arity: t => list((Position.t, Sort.t)) =
    fun
    | Root_root => []
    | Exp_var(_) => []
    | Exp_lam => [(Lam_param, Pat), (Lam_type, Typ), (Lam_body, Exp)]
    | Exp_app => [(App_fun, Exp), (App_arg, Exp)]
    | Exp_plus => [(Plus_left, Exp), (Plus_right, Exp)]
    | Exp_times => [(Times_left, Exp), (Times_right, Exp)]
    | Exp_num(_) => []
    | Pat_var(_) => []
    | Typ_arrow => [(Arrow_arg, Typ), (Arrow_result, Typ)]
    | Typ_num => [];

  let initial_position: t => option(Position.t) =
    fun
    | Root_root => Some(Root_root_root)
    | Exp_var(_) => None
    | Exp_lam => Some(Lam_body)
    | Exp_app => Some(App_fun)
    | Exp_plus => Some(Plus_left)
    | Exp_times => Some(Times_left)
    | Exp_num(_) => None
    | Pat_var(_) => None
    | Typ_arrow => Some(Arrow_arg)
    | Typ_num => None;

  let wrap_position: t => option(Position.t) =
    fun
    | Root_root => Some(Root_root_root)
    | Exp_var(_) => None
    | Exp_lam => Some(Lam_body)
    | Exp_app => Some(App_arg)
    | Exp_plus => Some(Plus_right)
    | Exp_times => Some(Times_right)
    | Exp_num(_) => None
    | Pat_var(_) => None
    | Typ_arrow => Some(Arrow_result)
    | Typ_num => None;
};

module Root = {
  let sort: Sort.t = Root;
  let constructor: Constructor.t = Root_root;
  let position: Position.t = Root_root_root;
};

module Show = {
  type t = Lang.show(Position.t);

  let position: Position.t => list(t) =
    fun
    | Root_root_root => [Str("root")]
    | Lam_param => [Str("param")]
    | Lam_type => [Str("type")]
    | Lam_body => [Str("body")]
    | App_fun => [Str("fun")]
    | App_arg => [Str("arg")]
    | Plus_left => [Str("left")]
    | Plus_right => [Str("right")]
    | Times_left => [Str("left")]
    | Times_right => [Str("right")]
    | Arrow_arg => [Str("arg")]
    | Arrow_result => [Str("result")];

  let constructor: Constructor.t => list(t) =
    fun
    | Root_root => []
    | Exp_var(x) => [Str(x)]
    | Exp_lam => [
        Str("λ"),
        Pos(Lam_param),
        Str(":"),
        Pos(Lam_type),
        Str("."),
        Pos(Lam_body),
      ]
    | Exp_app => [Pos(App_fun), Str(" "), Pos(App_arg)]
    | Exp_plus => [Pos(Plus_left), Str("+"), Pos(Plus_right)]
    | Exp_times => [Pos(Times_left), Str("*"), Pos(Times_right)]
    | Exp_num(n) => [Str(Int.to_string(n))]
    | Pat_var(x) => [Str(x)]
    | Typ_arrow => [Pos(Arrow_arg), Str("→"), Pos(Arrow_result)]
    | Typ_num => [Str("num")];
};
