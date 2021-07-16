open Sexplib0.Sexp_conv

module Sort = struct
  type t = Exp | Pat | Typ

  let equal s1 s2 = s1 = s2
end

module Position = struct
  type t =
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

  let sort : t -> Sort.t = function
    | RootRoot | LamBody | AppFun | AppArg | PlusLeft | PlusRight | TimesLeft
    | TimesRight ->
        Exp
    | LamParam -> Pat
    | LamType | ArrowArg | ArrowResult -> Typ

  let is_root : t -> bool = function RootRoot -> true | _ -> false
end

module Constructor = struct
  type t =
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

  let sort : t -> Sort.t = function
    | Root | ExpVar _ | ExpLam | ExpApp | ExpNum _ | ExpPlus | ExpTimes -> Exp
    | PatVar _ -> Pat
    | TypArrow | TypNum -> Typ

  let equal_sorts (k1 : t) (k2 : t) : bool = sort k1 = sort k2

  let default_position : t -> Position.t option = function
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
end

module PositionMap = Map.Make (struct
  type t = Position.t

  let compare = compare
end)

let arity (k : Constructor.t) : Sort.t PositionMap.t =
  let bindings : (Position.t * Sort.t) list =
    match k with
    | Root -> [ (RootRoot, Sort.Exp) ]
    | ExpVar _ -> []
    | ExpLam -> [ (LamParam, Pat); (LamType, Typ); (LamBody, Exp) ]
    | ExpApp -> [ (AppFun, Exp); (AppArg, Exp) ]
    | ExpPlus -> [ (PlusLeft, Exp); (PlusRight, Exp) ]
    | ExpTimes -> [ (TimesLeft, Exp); (TimesRight, Exp) ]
    | ExpNum _ | PatVar _ -> []
    | TypArrow -> [ (ArrowArg, Typ); (ArrowResult, Typ) ]
    | TypNum -> []
  in
  bindings |> List.to_seq |> PositionMap.of_seq

let is_well_sorted (source : Constructor.t) (position : Position.t)
    (target : Constructor.t) : bool =
  let arity = arity source in
  match PositionMap.find_opt position arity with
  | Some sort -> Sort.equal (Constructor.sort target) sort
  | None -> false
