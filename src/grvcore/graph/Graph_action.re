module type T = {
  type edge;

  type t = {
    state: Edge_state.t,
    edge,
  };
};

module Make =
       (
         L: Lang.T,
         V: Vertex.T with type constructor = L.Constructor.t,
         E: Edge.T with type position = L.Position.t and type vertex = V.t,
       )
       : T => {
  type edge = E.t;

  type t = {
    state: Edge_state.t,
    edge,
  };
};
