module type T = {
  type position;
  include Map.S with type key = position;
};

module Make = (L: Lang.T) : T => {
  type position = L.position;

  include Map.Make({
    type t = position;
    let compare = compare;
  });
};
