module type T = {
  type position;
  type vertex;

  type source = {
    position,
    vertex,
  };
  type t = {
    id: Id.t,
    source,
    target: vertex,
  };

  module Map: Map.S with type key = t;
  module Set: Set.S with type elt = t;
};

module Make =
       (L: Lang.T, V: Vertex.T with type constructor = L.constructor)
       : T => {
  type position = L.position;
  type vertex = V.t;

  type source = {
    position,
    vertex,
  };
  type t = {
    id: Id.t,
    source,
    target: vertex,
  };

  module OrderedType = {
    type nonrec t = t;
    let compare = compare;
  };

  module Map = Map.Make(OrderedType);
  module Set = Set.Make(OrderedType);
};
