module type T = {
  type position;
  include Map.S with type key = position;
};

module Make = (L: Lang.T) : (T with type position = L.Position.t) => {
  type position = L.Position.t;

  include Map.Make({
    type t = position;
    let compare = compare;
  });
};
