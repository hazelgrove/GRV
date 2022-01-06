module type T = {
  type vertex;
  type source;
  type edge;
  type graph;
  type position_map('t);

  type t =
    | Node(ast_node)
    | MultiChildConflict(list(t))
    | MultiParentConflict(edge)
    | CycleRoot(edge)
    | Hole(source)

  and ast_node = {
    vertex,
    parents: graph,
    children: position_map(list(t)),
  };
};

module Make =
       (
         L: Lang.T,
         V: Vertex.T with type constructor = L.constructor,
         E: Edge.T with type position = L.position and type vertex = V.t,
         G:
           Graph.T with
             type vertex = V.t and
             type source = E.source and
             type edge = E.t and
             type edge_set = E.Set.t,
         P: PositionMap.T with type position = L.position,
       )
       : T => {
  type vertex = E.vertex;
  type source = E.source;
  type edge = E.t;
  type graph = G.t;
  type position_map('t) = P.t('t);

  type t =
    | Node(ast_node)
    | MultiChildConflict(list(t))
    | MultiParentConflict(edge)
    | CycleRoot(edge)
    | Hole(source)

  and ast_node = {
    vertex,
    parents: graph,
    children: position_map(list(t)),
  };
};
