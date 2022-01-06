type t = int;

let compare: (t, t) => int = compare;

module Gen = {
  type id = t;
  type t = int;
  let init: t = 1;
  let next = (next: t): (id, t) => (next, next + 1);
};
