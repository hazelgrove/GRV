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

let rec reachable (graph : Graph.t) (mp : Vertex.Set.t) (v : Vertex.t) : tree =
  if Vertex.Set.mem v mp then Ref v.id
  else
    (* let trees = Vertex.Set.fold (fun v' trees -> reachable graph mp v :: trees) in *)
    Con
      ( v.value,
        Edge.Set.fold
          (fun e ts ->
            (e.value.source.index, reachable graph mp e.value.target) :: ts)
          (Graph.vertex_children graph v)
          [] )

(* else Con (v.value, Vertex.Set.map (reachable graph mp) (child_vertexes v)) *)

(* so we don't immediately stop on the first vertex, since they're all in mp already *)
(* let reachable' (graph : Graph.t) (mp : Vertex.Set.t) (v : Vertex.t) : tree =
 *   Con (con of v, Vertex.Set.map (reachable graph mp) (Graph.child_vertexes v)) *)

(* Returns option to enable backtracking *)
let find_a_cycle_vertex ?(seen = Vertex.Set.empty) (graph : Graph.t)
    (v : Vertex.t) : Vertex.t option =
  let parents = Graph.parent_vertexes graph v in
  let seen_parents = Vertex.Set.inter parents seen in
  if Vertex.Set.is_empty seen_parents then
    let seen = Vertex.Set.add v seen in
    Vertex.Set.find_first_opt (fun v' -> Vertex.Set.mem v' seen) seen_parents
  else Some (Vertex.Set.choose seen_parents)

(* Also returns option to enable backtracking *)
let rec find_all_cycle_vertexes ?(seen = Vertex.Set.empty) (graph : Graph.t)
    (v : Vertex.t) : Vertex.Set.t option =
  if Vertex.Set.mem v seen then Some seen
  else
    let seen = Vertex.Set.add v seen in
    Vertex.Set.fold
      (fun v' last ->
        match last with
        | Some _ -> last
        | None -> find_all_cycle_vertexes graph v' ~seen)
      (Graph.parent_vertexes graph v)
      None

let least_vertex : Vertex.Set.t -> Vertex.t = Vertex.Set.min_elt

let simple_cycle (graph : Graph.t) (mp : Vertex.Set.t) (v : Vertex.t) : tree =
  (* assume these options will always be Some thing *)
  let v' = Option.get (find_a_cycle_vertex graph v) in
  let cycle_vertexes = Option.get (find_all_cycle_vertexes graph v') in
  let cycle_root = least_vertex cycle_vertexes in
  reachable graph mp cycle_root

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
  let Graph.{ root; vertexes; multiparent; orphans; _ } = Graph.roots graph in
  let reachable_trees v ts = reachable graph multiparent v :: ts in
  let remaining = Vertex.Set.(diff vertexes (union multiparent orphans)) in
  ( reachable graph multiparent root,
    Vertex.Set.fold reachable_trees multiparent [],
    Vertex.Set.fold reachable_trees orphans [],
    simple_cycles graph multiparent remaining )
