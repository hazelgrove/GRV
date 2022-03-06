module Syntax = {
  let ( let* ) = Option.bind;
  let (let+) = (a_opt, f) => Option.map(f, a_opt);
};
