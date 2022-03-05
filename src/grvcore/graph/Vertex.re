module type T = {
  type constructor;

  type t = {
    id: Id.t,
    constructor,
  };

  let init: constructor => Id.Ctx.t(t);

  module Map: Map.S with type key = t;
};

module Make = (L: Lang.T) : (T with type constructor = L.Constructor.t) => {
  type constructor = L.Constructor.t;

  type t = {
    id: Id.t,
    constructor,
  };

  let init = (constructor: constructor): Id.Ctx.t(t) => {
    open Id.Ctx;
    let* id = next;
    return({id, constructor});
  };

  module Map =
    Map.Make({
      type nonrec t = t;
      let compare = compare;
    });
};
