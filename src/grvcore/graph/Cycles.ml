 (* very slow *)
let reachable (graph : Graph.t) (mp : Vertex.Set.t) (v : Vertex.t) : Vertex.Set.t =
  (* recomputing every time = slow *)
  let recur (v' : Vertex.t) : Vertex.Set.t =
    if Vertex.Set.member v' mp then
      Vertex.Set.empty
    else
      let descendants : Vertex.Set.t list = Vertex.Set.map (recur graph) (Graph.child_vertexes v') in
      (* many unions between large sets = slow b/c smallest set keeps getting bigger. Want to do incremental (e.g., unioning singleton sets every time) *)
      Vertex.Set.union (Vertex.Set.singleton v' :: descendants)
  in
  recur v

(* faster *)
type tree =
| Ref of Vertex.Id.t
| Con of Con * (position * tree) list

let rec reachable_tree (graph : Graph.t) (mp : Vertex.Set.t) (v : Vertex.t) : tree =
  if Vertex.Set.mem v mp then
    Ref (id of v)
  else
    Con (con of v, Vertex.Set.map (reachable_tree graph mp) (Graph.child_vertexes v))

(* so we don't immediately stop on the first vertex, since they're all in mp already *)
let reachable_tree' (graph : Graph.t) (mp : Vertex.Set.t) (v : Vertex.t) : tree =
  Con (con of v, Vertex.Set.map (reachable_tree graph mp) (Graph.child_vertexes v))

let find_cycle_vertex

let find_all_cycle_vertexes

let choose_root_cycle_vertex

let simple_cycle (graph : Graph.t) (v : Vertex.t) : tree =
  let v' = find_cycle_vertex graph v in
  let cycle_vertexes = find_all_cycle_vertexes graph v' in
  let cycle_root = choose_root_cycle_vertex cycle_vertexes in
  reachable_tree graph mp cycle_root

let simple_cycles (graph : Graph.t) (remaining : Vertex.Set.t) : tree list =
  if Vertex.set.is_empty remaining then
    []
  else
    let v = Vertex.Set.choose remaining in
    let this_tree = simple_cycle graph v in
    let remaining' = subtract_tree_vertexes remaining this_tree in
    this_tree :: simple_cycles graph remaining'

let render (graph : Graph.t) : tree * tree list * tree list * tree list =
  let mp : Vertex.Set.t = multiparents graph in
  let orphans : Vertex.Set.t = orphans graph in
  let rooted_tree = reachable_tree graph mp graph.root_vertex in
  let mp_trees = Vertex.Set.map (reachable_tree' graph mp) mp in
  let orphan_trees = Vertex.Set.map (reachable_tree graph mp) orphans in
  let simple_cycle_trees =
    let f = ... in
    f () 
  in
  (rooted_tree, mp_trees, orphan_trees, simple_cycle_trees)