type exp = Var of string | Lam | App | Num of int | Plus | Times

type pat = Var of string

type typ = Arrow | Num

type t = Root | Exp of exp | Pat of pat | Typ of typ

let sort : t -> Sort.t = function
  | Root | Exp _ -> Exp
  | Pat _ -> Pat
  | Typ _ -> Typ

let arity : t -> Arity.t = function
  | Root -> Arity.singleton Root
  | Exp (Var _) -> Arity.empty
  | Exp Lam -> Arity.of_list [ Exp (Lam Param); Exp (Lam Type); Exp (Lam Body) ]
  | Exp App -> Arity.of_list [ Exp (App Fun); Exp (App Arg) ]
  | Exp Plus -> Arity.of_list [ Exp (Plus Left); Exp (Plus Right) ]
  | Exp Times -> Arity.of_list [ Exp (Times Left); Exp (Times Right) ]
  | Exp (Num _) -> Arity.empty
  | Pat (Var _) -> Arity.empty
  | Typ Arrow -> Arity.of_list [ Typ (Arrow Arg); Typ (Arrow Result) ]
  | Typ Num -> Arity.empty

let default_position : t -> Position.t option = function
  | Root -> Some Root
  | Exp (Var _) -> None
  | Exp Lam -> Some (Exp (Lam Param))
  | Exp App -> Some (Exp (App Fun))
  | Exp (Num _) -> None
  | Exp Plus -> Some (Exp (Plus Left))
  | Exp Times -> Some (Exp (Times Left))
  | Pat (Var _) -> None
  | Typ Arrow -> Some (Typ (Arrow Arg))
  | Typ Num -> None
