module type T = {
  type position;
  type vertex;

  type t = {
    position,
    vertex,
  };

  let init: Id.Ctx.t(t);
};

module Make =
       (L: Lang.T, V: Vertex.T with type constructor = L.Constructor.t)
       : (T with type position = L.Position.t and type vertex = V.t) => {
  type position = L.Position.t;
  type vertex = V.t;

  type t = {
    position,
    vertex,
  };

  let init: Id.Ctx.t(t) = {
    open Id.Ctx;
    let+ vertex = V.init(L.Root.constructor);
    {vertex, position: L.Root.position};
  };
};
