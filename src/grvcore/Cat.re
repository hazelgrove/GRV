/*
 A SEMIGROUP is a type `t` with an associative binary operation `append`.
 */
module type SEMIGROUP = {
  type t;
  let append: (t, t) => t;
};

/*
 A MONOID is a SEMIGROUP with an `identity` value.
 */
module type MONOID = {
  include SEMIGROUP;
  let identity: t;
};

/*
 A FUNCTOR is a mapping `let+` from one type `t('a)` to another `t('b)` that
 preserves identity and composition.
 */
module Functor = {
  module type T = {
    type t('a);
    let fmap: (t('a), 'a => 'b) => t('b);
  };

  module type S = {
    include T;
    let (let+): (t('a), 'a => 'b) => t('b);
  };

  module Make = (F: T) : (S with type t('a) = F.t('a)) => {
    include F;
    let (let+) = F.fmap;
  };
};

/*
 An APPLICATIVE is a FUNCTOR from one MONOID `t('a)` to another `t('b)`
 with a lifting function `pure` and a sequencing operator `<*>`.
 */
module Applicative = {
  module type T = {
    type t('a);
    let pure: 'a => t('a);
    let (<*>): (t('a), t('b)) => t(('a, 'b));
  };

  module type S = {
    include T;
    let (and+): (t('a), t('b)) => t(('a, 'b));
  };

  module Make = (A: T) : (S with type t('a) = A.t('a)) => {
    include A;
    let (and+): (t('a), t('b)) => t(('a, 'b)) = (<*>);
  };
};

/*
 A MONAD is an APPLICATIVE with a sequential composition function `let*` such
 that:

 1. `return` is a left-identity for `let*`

    (let*)(return(x), f) == f(X)

 2. `return` is a right-identity for `let*`

    (let*)(m, return) == m

 3. `let*` is associative

    (let*)(m, x => (let*)(f(x), g)) == (let*)((let*)(m, f), g)
 */
module Monad = {
  module type T = {
    type t('a);
    let return: 'a => t('a);
    let (>>=): (t('a), 'a => t('b)) => t('b);
  };

  module type S = {
    include T;
    let ( let* ): (t('a), 'a => t('b)) => t('b);
  };

  module Make = (M: T) : (S with type t('a) = M.t('a)) => {
    include M;
    let ( let* ): (t('a), 'a => t('b)) => t('b) = (>>=);
    // let ( and* ): (t('a), t('b)) => t(('a, 'b));
  };
};

/*
 A STATE_MONAD is a MONAD on pairs of type `('a, state)` with an accessor `get`
 and an updater `put`.
 */
module State_Monad = {
  module type S = {
    type state;
    type t('a) = state => ('a, state);
    include Monad.S with type t('a) := t('a);
    let get: t(state);
    let put: state => t(unit);
    let gets: (state => 'b) => t('b);
    let run: (t('a), state) => ('a, state);
    let eval: (t('a), state) => 'a;
    let exec: (t('a), state) => state;
  };

  module Make =
         (State: MONOID)

           : (
             S with
               type state = State.t and type t('a) = State.t => ('a, State.t)
         ) => {
    type state = State.t;
    type t('a) = state => ('a, state);

    module M: Monad.T with type t('a) = t('a) = {
      type t('a) = state => ('a, state);
      let return: 'a => t('a) = (x, state) => (x, state);
      let (>>=): (t('a), 'a => t('b)) => t('b) =
        (m, f, state) => m(state) |> (((x, state')) => f(x, state'));
    };
    include (Monad.Make(M): Monad.S with type t('a) := t('a));

    let get: t(state) = state => (state, state);
    let put: state => t(unit) = (state, _) => ((), state);
    let gets: (state => 'a) => t('a) =
      f => get >>= (state => return(f(state)));
    let run: (t('a), state) => ('a, state) = (m, state) => m(state);
    let eval: (t('a), state) => 'a = (m, state) => run(m, state) |> fst;
    let exec: (t('a), state) => state = (m, state) => run(m, state) |> snd;
  };
};
