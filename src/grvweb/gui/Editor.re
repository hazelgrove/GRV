open CoreImpl;

type t = {
  id: Id.t,
  graph: Graph.t,
  cursor: Cursor.t,
  pending_actions: list(Graph_action.t),
  action_history: list(Graph_action.t),
};

let equal = (model1: t, model2: t): bool => Id.equal(model1.id, model2.id);

let init: Id.Ctx.t(t) = {
  open Id.Ctx;
  let* id = next;
  let* cursor = Cursor.init;
  let graph = Graph.empty;
  return({id, graph, cursor, pending_actions: [], action_history: []});
};
