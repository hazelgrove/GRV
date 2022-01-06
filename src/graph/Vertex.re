module type T = {
  type constructor;

  type t = {
    id: Id.t,
    constructor,
  };

  module Map: Map.S with type key = t;
};

module Make = (L: Lang.T) : T => {
  type constructor = L.constructor;

  type t = {
    id: Id.t,
    constructor,
  };

  module Map =
    Map.Make({
      type nonrec t = t;
      let compare = compare;
    });
};
