module type T = {
  type graph;
  type term;

  type t = {
    orphans: list(term),
    multiparents: list(term),
    cycles: list(term),
  };

  let decompose: graph => t;
  let recompose: t => graph;
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
         T:
           Term.T with
             type vertex = V.t and
             type source = E.source and
             type edge = E.t and
             type graph = G.t and
             type position_map('t) = P.t('t),
       )
       : T => {
  type graph = G.t;
  type term = T.t;

  type t = {
    orphans: list(term),
    multiparents: list(term),
    cycles: list(term),
  };

  type edge = E.t;

  let decompose = (graph: graph): t => {
    let rec decompose_child = (position, edge: edge): term => {
      let vertex = edge.target;
      let source = E.{position, vertex};
      let out_edges = graph |> G.out_adjacent_plus_edges(source);
      switch (out_edges) {
      | [] => Hole(source)
      | [out_edge] =>
        switch (graph |> G.min_ancestor_root(out_edge.target)) {
        | None => failwith(__LOC__ ++ ": impossible")
        | Some(root_vertex) =>
          root_vertex == out_edge.target
            ? T.CycleRoot(edge) : decompose_edge(out_edge)
        }
      | _ => T.MultiChildConflict(out_edges |> List.map(decompose_edge))
      };
    }
    and decompose_edge = (edge: edge): term => {
      let vertex = edge.target;
      let parents = graph |> G.in_adjacent_bindings(edge.target) |> G.of_list;
      let children =
        edge.target.constructor
        |> L.arity
        |> List.map(((position, _)) => {
             let source = E.{position, vertex: edge.target};
             let children =
               graph
               |> G.out_adjacent_bindings(source)
               |> List.map(((edge, _)) => decompose_child(position, edge));
             (position, children);
           })
        |> List.to_seq
        |> P.of_seq;
      T.Node({vertex, parents, children});
    };

    let (orphaned_edges, multiparented_edges, cycle_edges) =
      G.root_edges(graph);
    let orphans = orphaned_edges |> List.map(decompose_edge);
    let multiparents = multiparented_edges |> List.map(decompose_edge);
    let cycles = cycle_edges |> List.map(decompose_edge);
    {orphans, multiparents, cycles};
  };

  let recompose = (grove: t): graph => {
    let rec recompose_term: term => graph =
      fun
      | Node(ast_node) => ast_node.parents
      | MultiChildConflict(children) =>
        children |> List.map(recompose_term) |> G.concat
      | MultiParentConflict(edge) => G.of_list([(edge, Plus)])
      | CycleRoot(edge) => G.of_list([(edge, Plus)])
      | Hole(_) => G.empty;
    let recompose_terms = (terms: list(term)): graph =>
      terms |> List.map(term => term |> recompose_term) |> G.concat;
    let os = recompose_terms(grove.orphans);
    let ms = recompose_terms(grove.multiparents);
    let cs = recompose_terms(grove.cycles);
    G.concat([os, ms, cs]);
  };
};
