open Sexplib0.Sexp_conv

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
[@@deriving sexp]

let sort_of : constructor -> sort = function
  | Root | ExpVar _ | ExpLam | ExpApp | ExpNum _ | ExpPlus | ExpTimes -> Exp
  | PatVar _ -> Pat
  | TypArrow | TypNum -> Typ

let sorts_equal (k1 : constructor) (k2 : constructor) : bool =
  sort_of k1 = sort_of k2

type position =
  | RootRoot
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
[@@deriving sexp]

let default_position : constructor -> position option = function
  | Root -> Some RootRoot
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
  type t = position * sort

  let compare = compare
end)

let arity : constructor -> Arity.t = function
  | Root -> Arity.singleton (RootRoot, Exp)
  | ExpVar _ -> Arity.empty
  | ExpLam -> Arity.of_list [ (LamParam, Pat); (LamType, Typ); (LamBody, Exp) ]
  | ExpApp -> Arity.of_list [ (AppFun, Exp); (AppArg, Exp) ]
  | ExpPlus -> Arity.of_list [ (PlusLeft, Exp); (PlusRight, Exp) ]
  | ExpTimes -> Arity.of_list [ (TimesLeft, Exp); (TimesRight, Exp) ]
  | ExpNum _ -> Arity.empty
  | PatVar _ -> Arity.empty
  | TypArrow -> Arity.of_list [ (ArrowArg, Typ); (ArrowResult, Typ) ]
  | TypNum -> Arity.empty

let is_valid_position (position : position) (inconstructor : constructor)
    (constructor : constructor) : bool =
  Arity.mem (position, sort_of inconstructor) (arity constructor)
