(* Lang *)

module Sort = struct
  type t = Exp | Pat | Typ
end

module Position = struct
  type lam = Param | Type | Body

  type app = Fun | Arg

  type plus = Left | Right

  type times = Left | Right

  type arrow = Arg | Result

  type exp = Lam of lam | App of app | Plus of plus | Times of times

  type typ = Arrow of arrow

  type t = Root | Exp of exp | Typ of typ

  let sort : t -> Sort.t = function Root | Exp _ -> Exp | Typ _ -> Typ
end

module Arity = Set.Make (struct
  type t = Position.t

  let compare = compare
end)

module Constructor = struct
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
    | Exp Lam ->
        Arity.of_list [ Exp (Lam Param); Exp (Lam Type); Exp (Lam Body) ]
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
end

(* Graph *)

module Id = struct
  type t = int
end

module Vertex = struct
  type t = { id : Id.t; constructor : Constructor.t }
end

module Edge_state = struct
  type t = Plus | Minus
end

module Edge = struct
  type t = {
    id : Id.t;
    source : Vertex.t;
    position : Position.t;
    target : Vertex.t;
  }

  module Map = Map.Make (struct
    type t = Edge_state.t

    let compare = compare
  end)
end

module Graph = struct
  type t = Edge_state.t Edge.Map.t
end

(* Term *)

module Conflict (T : sig
  type t

  val compare : t -> t -> int
end) =
struct
  include Set.Make (struct
    type t = T.t

    let compare = T.compare
  end)
end

module Pat = struct
  module T = struct
    type t = Var of Graph.t * string

    let compare = compare
  end

  module Conflict = Conflict (T)

  type t = Pat of T.t | PatConflict of Conflict.t
end

module Typ = struct
  module T = struct
    type t = Arrow of Graph.t * t * t | Num of Graph.t

    let compare = compare
  end

  module Conflict = Conflict (T)

  type t = Typ of T.t | TypConflict of Conflict.t
end

module Exp = struct
  module T = struct
    type t =
      | Var of Graph.t
      | Lam of Graph.t * Pat.t * Typ.t * t
      | App of Graph.t

    let compare = compare
  end

  module Conflict = Conflict (T)

  type t = Exp of T.t | ExpConflict of Conflict.t
end
