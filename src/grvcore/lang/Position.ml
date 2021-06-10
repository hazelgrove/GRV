type lam = Param | Type | Body

type app = Fun | Arg

type plus = Left | Right

type times = Left | Right

type arrow = Arg | Result

type exp = Lam of lam | App of app | Plus of plus | Times of times

type typ = Arrow of arrow

type t = Root | Exp of exp | Typ of typ

let sort_of : t -> Sort.t = function Root | Exp _ -> Exp | Typ _ -> Typ
