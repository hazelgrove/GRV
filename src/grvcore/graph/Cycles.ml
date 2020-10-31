let child_vertexes (graph : Graph.t) (v : Vertex.t) : Vertex.Set.t =
  Edge.Set.fold
    (fun e vs -> Vertex.Set.add e.value.target vs)
    (Graph.vertex_children graph v)
    Vertex.Set.empty

(* very slow *)
let naive_reachable (graph : Graph.t) (mp : Vertex.Set.t) (v : Vertex.t) :
    Vertex.Set.t =
  (* recomputing every time = slow *)
  let rec recur (v' : Vertex.t) : Vertex.Set.t =
    if Vertex.Set.mem v' mp then Vertex.Set.empty
    else
      let descendants : Vertex.Set.t list =
        Vertex.Set.fold
          (fun v'' vs -> recur v'' :: vs)
          (child_vertexes graph v') []
        (* Vertex.Set.fold
         *   (fun vertex vertexes -> Vertex.Set.union (recur vertex) vertexes)
         *   (child_vertexes graph v') Vertex.Set.empty *)
      in
      (* many unions between large sets = slow b/c smallest set keeps getting
         bigger. Want to do incremental (e.g., unioning singleton sets every
         time)
      *)
      List.fold_right Vertex.Set.union
        (Vertex.Set.singleton v' :: descendants)
        Vertex.Set.empty
  in
  recur v

(* faster *)
type tree =
  | Ref of Uuid.Id.t
  | Con of Lang.Constructor.t * (Lang.Index.t * tree) list

let rec reachable_tree (graph : Graph.t) (mp : Vertex.Set.t) (v : Vertex.t) :
    tree =
  if Vertex.Set.mem v mp then Ref v.id
  else
    (* let trees = Vertex.Set.fold (fun v' trees -> reachable_tree graph mp v :: trees) in *)
    Con
      ( v.value,
        Edge.Set.fold
          (fun e ts ->
            (e.value.source.index, reachable_tree graph mp e.value.target) :: ts)
          (Graph.vertex_children graph v)
          [] )

(* else Con (v.value, Vertex.Set.map (reachable_tree graph mp) (child_vertexes v)) *)

(* so we don't immediately stop on the first vertex, since they're all in mp already *)
(* let reachable_tree' (graph : Graph.t) (mp : Vertex.Set.t) (v : Vertex.t) : tree =
 *   Con (con of v, Vertex.Set.map (reachable_tree graph mp) (Graph.child_vertexes v)) *)

let find_a_cycle_vertex (graph : Graph.t) ?(seen = Vertex.Set.empty)
    (v : Vertex.t) : Vertex.t =
  let parents =
    Edge.Set.fold
      (fun e vs -> Vertex.Set.add e.value.source.vertex vs)
      (Graph.parents graph v) Vertex.Set.empty
  in
  if Vertex.Set.disjoint parents seen then
    Vertex.Set.find_first
      (fun v' -> Vertex.Set.mem v' seen)
      (Vertex.Set.inter parents seen)
  else Vertex.Set.choose (Vertex.Set.inter parents seen)

let[@warning "-27"] find_all_cycle_vertexes (graph : Graph.t) (v : Vertex.t) :
    Vertex.Set.t =
  failwith __LOC__

let[@warning "-27"] choose_root_cycle_vertex (vertexes : Vertex.Set.t) :
    Vertex.t =
  failwith __LOC__

let simple_cycle (graph : Graph.t) (mp : Vertex.Set.t) (v : Vertex.t) : tree =
  let v' = find_a_cycle_vertex graph v in
  let cycle_vertexes = find_all_cycle_vertexes graph v' in
  let cycle_root = choose_root_cycle_vertex cycle_vertexes in
  reachable_tree graph mp cycle_root

let rec tree_vertexes : tree -> Vertex.t list = function
  | Ref _ -> []
  | Con (_, children) ->
      let _, ts = List.split children in
      List.concat (List.map tree_vertexes ts)

let subtract_tree_vertexes (vs : Vertex.Set.t) (t : tree) : Vertex.Set.t =
  Vertex.Set.diff vs (Vertex.Set.of_list (tree_vertexes t))

let rec simple_cycles (graph : Graph.t) (mp : Vertex.Set.t)
    (remaining : Vertex.Set.t) : tree list =
  if Vertex.Set.is_empty remaining then []
  else
    let v = Vertex.Set.choose remaining in
    let this_tree = simple_cycle graph mp v in
    let remaining' = subtract_tree_vertexes remaining this_tree in
    this_tree :: simple_cycles graph mp remaining'

let render (graph : Graph.t) : tree * tree list * tree list * tree list =
  let cache = Graph.roots graph in
  let rooted_tree = reachable_tree graph cache.multiparent cache.root in
  let mp_trees =
    Vertex.Set.fold
      (fun v ts -> reachable_tree graph cache.multiparent v :: ts)
      cache.multiparent []
  in
  let orphan_trees =
    Vertex.Set.fold
      (fun v ts -> reachable_tree graph cache.multiparent v :: ts)
      cache.orphans []
  in
  let remaining =
    Vertex.Set.(
      diff (diff (Graph.vertexes graph) cache.multiparent) cache.orphans)
  in
  let simple_cycle_trees = simple_cycles graph cache.multiparent remaining in
  (rooted_tree, mp_trees, orphan_trees, simple_cycle_trees)
