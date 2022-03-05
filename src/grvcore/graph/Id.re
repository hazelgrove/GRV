open Cat;

type id = int;
type t = id;

let to_string: t => string = Int.to_string;

let compare: (t, t) => int = compare;

let equal: (t, t) => bool = Int.equal;

module Gen = {
  type t = int;
  let init: t = 1;
  let next = (gen: t): (id, t) => (gen, gen + 1);
  // MONOID
  let append: (t, t) => t = max;
  let identity: t = init;
};

module Ctx = {
  include State_Monad.Make((Gen: MONOID with type t = Gen.t));

  type ctx('a) = t('a);

  let next: t(Gen.t) = Gen.next;

  include Applicative.Make({
    type t('a) = ctx('a);
    let pure: 'a => t('a) = return;
    let (<*>): (t('a), t('b)) => t(('a, 'b)) =
      (x, y) => {
        let* x = x;
        let* y = y;
        return((x, y));
      };
  });

  include Cat.Functor.Make({
    type t('a) = ctx('a);

    let fmap: (t('a), 'a => 'b) => t('b) =
      (x, f) => {
        let* x = x;
        return(f(x));
      };
  });
};
