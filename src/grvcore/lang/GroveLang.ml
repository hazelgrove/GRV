type sort = Exp | Pat | Typ

type constructor =
  | Root
  | ExpVar of string
  | ExpLam
  | ExpApp
  | ExpNum of int
  | ExpPlus
  | ExpTimes
  | PatVar of string
  | TypArrow
  | TypNum

let sort_of_constructor : constructor -> sort = function
  | Root | ExpVar _ | ExpLam | ExpApp | ExpNum _ | ExpPlus | ExpTimes -> Exp
  | PatVar _ -> Pat
  | TypArrow | TypNum -> Typ

type position =
  | Root
  | LamParam
  | LamType
  | LamBody
  | AppFun
  | AppArg
  | PlusLeft
  | PlusRight
  | TimesLeft
  | TimesRight
  | ArrowArg
  | ArrowResult

let sort_of_position : position -> sort = function
  | Root | LamParam | LamType | LamBody | AppFun | AppArg | PlusLeft | PlusRight
  | TimesLeft | TimesRight ->
      Exp
  | ArrowArg | ArrowResult -> Typ

let default_position : constructor -> position option = function
  | Root -> Some Root
  | ExpVar _ -> None
  | ExpLam -> Some LamParam
  | ExpApp -> Some AppFun
  | ExpNum _ -> None
  | ExpPlus -> Some PlusLeft
  | ExpTimes -> Some TimesLeft
  | PatVar _ -> None
  | TypArrow -> Some ArrowArg
  | TypNum -> None

module Arity = Set.Make (struct
  type t = position

  let compare = compare
end)

let arity : constructor -> Arity.t = function
  | Root -> Arity.singleton Root
  | ExpVar _ -> Arity.empty
  | ExpLam -> Arity.of_list [ LamParam; LamType; LamBody ]
  | ExpApp -> Arity.of_list [ AppFun; AppArg ]
  | ExpPlus -> Arity.of_list [ PlusLeft; PlusRight ]
  | ExpTimes -> Arity.of_list [ TimesLeft; TimesRight ]
  | ExpNum _ -> Arity.empty
  | PatVar _ -> Arity.empty
  | TypArrow -> Arity.of_list [ ArrowArg; ArrowResult ]
  | TypNum -> Arity.empty
