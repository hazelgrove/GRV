module type T = {
  type sort;
  type constructor;
  type position;
  let sort: constructor => sort;
  let arity: constructor => list((position, sort));
  let default_position: constructor => option((position, position));
};
