type edge_state =
  | Bot
  | Plus
  | Minus;

module type T = {
  type vertex;
  type source;
  type edge;
  type edge_set;

  type t;

  let empty: t;
  let of_list: list((edge, edge_state)) => t;
  let concat: list(t) => t;
  let bindings: t => list((edge, edge_state));
  let find: (edge, t) => edge_state;
  let edges: t => list(edge);
  let plus_edges: t => list(edge);
  let root_edges: t => (list(edge), list(edge), list(edge));
  let children: (vertex, t) => list(edge);
  let in_adjacent_bindings: (vertex, t) => list((edge, edge_state));
  let out_adjacent_bindings: (source, t) => list((edge, edge_state));
  let in_adjacent_plus_edges: (vertex, t) => list(edge);
  let out_adjacent_plus_edges: (source, t) => list(edge);
  let ancestors: (~seen: edge_set=?, vertex, t) => edge_set;
  let min_ancestor: (vertex, t) => option(edge);
  let min_ancestor_root: (vertex, t) => option(vertex);
};

module Make =
       (
         L: Lang.T,
         V: Vertex.T with type constructor = L.constructor,
         E: Edge.T with type position = L.position and type vertex = V.t,
       )
       : T => {
  type vertex = E.vertex;
  type source = E.source;
  type edge = E.t;
  type edge_set = E.Set.t;

  type t = E.Map.t(edge_state);

  let empty: t = E.Map.empty;

  let of_list = (bindings: list((edge, edge_state))): t =>
    bindings |> List.to_seq |> E.Map.of_seq;

  let concat = (graphs: list(t)): t =>
    graphs
    |> List.map(E.Map.bindings)
    |> List.concat
    |> List.to_seq
    |> E.Map.of_seq;

  let bindings = (graph: t): list((edge, edge_state)) =>
    E.Map.bindings(graph);

  let find = (edge: edge, graph: t): edge_state =>
    graph |> E.Map.find_opt(edge) |> Option.value(~default=Bot);

  let edges = (graph: t): list(edge) =>
    graph |> E.Map.bindings |> List.map(fst);

  let plus_edges = (graph: t): list(edge) =>
    graph
    |> E.Map.bindings
    |> List.filter_map(((edge: edge, _)) =>
         graph |> find(edge) == Plus ? Some(edge) : None
       );

  let in_adjacent_bindings =
      (target: vertex, graph: t): list((edge, edge_state)) =>
    graph
    |> bindings
    |> List.filter(((edge: edge, _)) => target == edge.target);

  let out_adjacent_bindings =
      (source: source, graph: t): list((edge, edge_state)) =>
    graph
    |> bindings
    |> List.filter(((edge: edge, _)) => source == edge.source);

  let in_adjacent_plus_edges = (target: vertex, graph: t): list(edge) =>
    graph
    |> in_adjacent_bindings(target)
    |> List.filter_map(((edge, edge_state)) =>
         edge_state == Plus ? Some(edge) : None
       );

  let out_adjacent_plus_edges = (source: source, graph: t): list(edge) =>
    graph |> plus_edges |> List.filter((edge: edge) => source == edge.source);

  type in_degree =
    | Zero
    | One
    | Many;

  let root_edges = (graph: t): (list(edge), list(edge), list(edge)) => {
    let in_degrees: V.Map.t(in_degree) =
      graph
      |> E.Map.bindings
      |> List.fold_left(
           (in_degrees, (edge: edge, edge_state)) =>
             switch (edge_state) {
             | Bot => failwith(__LOC__ ++ ": impossible")
             | Plus =>
               in_degrees
               |> V.Map.update(
                    edge.target,
                    fun
                    | None => Some(Zero)
                    | Some(Zero) => Some(One)
                    | Some(One) => Some(Many)
                    | Some(_) as x => x,
                  )
             | Minus => in_degrees
             },
           V.Map.empty,
         );
    graph
    |> edges
    |> List.fold_left(
         ((os, ms, cs), edge: edge) =>
           switch (in_degrees |> V.Map.find_opt(edge.target)) {
           | None => failwith(__LOC__ ++ ": impossible")
           | Some(Zero) => ([edge, ...os], ms, cs)
           | Some(Many) => (os, [edge, ...ms], cs)
           | Some(One) => (os, ms, [edge, ...cs])
           },
         ([], [], []),
       );
  };

  let children = (vertex: vertex, graph: t): list(edge) =>
    graph
    |> plus_edges
    |> List.filter((edge: edge) => vertex == edge.source.vertex);

  let rec ancestors =
          (~seen: edge_set=E.Set.empty, vertex: vertex, graph: t): edge_set => {
    let child_edges = graph |> children(vertex);
    child_edges == []
      ? seen
      : child_edges
        |> List.fold_left(
             (seen, edge: edge) => graph |> ancestors(edge.target, ~seen),
             seen,
           );
  };

  let min_ancestor = (vertex: vertex, graph: t): option(edge) =>
    graph |> ancestors(vertex) |> E.Set.min_elt_opt;

  let min_ancestor_root = (vertex: vertex, graph: t): option(vertex) =>
    graph
    |> min_ancestor(vertex)
    |> Option.map((edge: edge) => edge.source.vertex);
};
