(* let child_vertexes (graph : Graph.t) (v : Vertex.t) : Vertex.Set.t =
  Edge.Set.fold
    (fun e vs -> Vertex.Set.add e.value.target vs)
    (Graph.vertex_children graph v)
    Vertex.Set.empty *)

(* very slow *)

(* let naive_reachable (graph : Graph.t) (mp : Vertex.Set.t) (v : Vertex.t) :
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
  recur v *)

(* faster *)

module IndexMap = Map.Make (struct
  type t = Lang.Index.t

  let compare = compare
end)

type t = Ref of Uuid.Id.t | Con of Vertex.t * t list IndexMap.t

let rec show : t -> string = function
  | Ref id -> "#" ^ Format.asprintf "%a" Uuid.Id.pp id
  | Con (vertex, children) ->
      "[" ^ Vertex.show vertex ^ "={"
      ^ IndexMap.fold
          (fun index trees str ->
            str ^ Lang.Index.show index ^ "=["
            ^ String.concat ", " (List.map show trees)
            ^ "]; ")
          children ""
      ^ "}]"

let rec reachable ?(seen : Vertex.Set.t = Vertex.Set.empty) (graph : Graph.t)
    (multiparent : Vertex.Set.t) (vertex : Vertex.t) : t =
  if Vertex.Set.(mem vertex multiparent || mem vertex seen) then Ref vertex.id
  else
    let seen = Vertex.Set.add vertex seen in
    let children =
      Edge.Set.fold
        (fun edge children ->
          let tree = reachable ~seen graph multiparent edge.value.target in
          IndexMap.update edge.value.source.index
            (function
              | None -> Some [ tree ] | Some trees -> Some (tree :: trees))
            children)
        (Graph.vertex_children graph vertex)
        IndexMap.empty
    in
    Con (vertex, IndexMap.map List.rev children)

(* Returns option to enable backtracking OR to indicate failure *)
let rec find_a_cycle_vertex ?(seen = Vertex.Set.empty) (graph : Graph.t)
    (vertex : Vertex.t) : Vertex.t option =
  Format.printf "seen = ";
  Vertex.print_set seen;
  Format.printf "]%!";
  if Vertex.Set.mem vertex seen then Some vertex
  else
    let parents = Graph.parent_vertexes graph vertex in
    Format.printf "parents = ";
    Vertex.print_set parents;
    Format.printf "]%!";
    let seen = Vertex.Set.add vertex seen in
    Vertex.Set.fold
      (fun v cycle_vertex_opt ->
        match cycle_vertex_opt with
        | Some _ -> cycle_vertex_opt
        | None -> find_a_cycle_vertex ~seen graph v)
      parents None

(* Returns option to enable backtracking ONLY. Every result is
   Some(set containing vertex). *)
let rec find_all_cycle_vertexes ?(seen = Vertex.Set.empty) (graph : Graph.t)
    (vertex : Vertex.t) : Vertex.Set.t =
  if Vertex.Set.mem vertex seen then seen
  else
    let seen = Vertex.Set.add vertex seen in
    let parents = Graph.parent_vertexes graph vertex in
    Vertex.Set.fold
      (fun v parent_vertex_set ->
        Vertex.Set.union parent_vertex_set
          (find_all_cycle_vertexes ~seen graph v))
      parents Vertex.Set.empty

let least_vertex : Vertex.Set.t -> Vertex.t = Vertex.Set.min_elt

(* A simple cycle is a set of vertices connected by a closed path, consisting
   of:

   1) two or more vertices, all descendants of each other, and
   2) the vertices of any non-multiparent subtrees reachable from the core, and
      not reachable from the root vertex.

  To determine the simple cycles of a graph, we:
  
  1) check if any vertex in the graph is a descendant of itself or some other
     vertex that is (a descendant of itself),
  2) find the least vertex in the core of this new(ly discovered) cycle, and
  3) return a tree connecting every vertex reachable from the least (vertex).

  The least vertex of a cycle is the one that was created first. If we begin
  our search for reachable vertices at the least vertex in a cycle, we know
  that <...>

*)
let simple_cycle (graph : Graph.t) (multiparent : Vertex.Set.t)
    (vertex : Vertex.t) : t option =
  Format.printf "vertex = %s%!" (Vertex.show vertex);
  let%map.Util.Option cycle_vertex = find_a_cycle_vertex graph vertex in
  (* we can always assume cycle_vertexes will be Some(non-empty set) because
     it always contains the cycle_vertex *)
  Format.printf "cycle_vertex = %s%!" (Vertex.show cycle_vertex);
  let cycle_vertexes = find_all_cycle_vertexes graph cycle_vertex in
  Format.printf "cycle_vertexes = ";
  Vertex.print_set cycle_vertexes;
  Format.printf "%!";
  let cycle_root = least_vertex cycle_vertexes in
  Format.printf "cycle_root = %s%!" (Vertex.show cycle_root);
  reachable graph multiparent cycle_root

let rec tree_vertexes (graph : Graph.t) (tree : t) : Vertex.t list =
  match tree with
  | Ref id -> [ Option.get (Graph.vertex graph id) ]
  | Con (vertex, children) ->
      IndexMap.fold
        (fun _ trees vertexes ->
          List.concat
            [
              vertexes;
              vertex :: List.concat (List.map (tree_vertexes graph) trees);
            ])
        children [ vertex ]

let subtract_tree_vertexes (graph : Graph.t) (vertexes : Vertex.Set.t)
    (tree : t) : Vertex.Set.t =
  Vertex.Set.diff vertexes (Vertex.Set.of_list (tree_vertexes graph tree))

let rec simple_cycles (graph : Graph.t) (multiparent : Vertex.Set.t)
    (remaining : Vertex.Set.t) : t list =
  Format.printf "remaining: ";
  Vertex.print_set remaining;
  Format.printf "%!";
  if Vertex.Set.is_empty remaining then []
  else
    let vertex = Vertex.Set.choose remaining in
    match simple_cycle graph multiparent vertex with
    | None ->
        simple_cycles graph multiparent (Vertex.Set.remove vertex remaining)
    | Some tree ->
        let cycle_trees =
          simple_cycles graph multiparent
            (subtract_tree_vertexes graph remaining tree)
        in
        tree :: cycle_trees

let decompose (graph : Graph.t) (multiparent : Vertex.Set.t) :
    t * t list * t list * t list =
  let vertexes = Graph.vertexes graph in
  let deleted = Vertex.Set.remove Vertex.root (Graph.orphans graph) in
  let reachable_tree = reachable graph multiparent Vertex.root in
  let reachable_vertexes =
    Vertex.Set.of_list (tree_vertexes graph reachable_tree)
  in
  let remaining =
    Vertex.Set.(
      diff
        (diff vertexes (union multiparent deleted) |> remove Vertex.root)
        reachable_vertexes)
  in
  Format.printf "\nvertexes: ";
  Vertex.print_set vertexes;
  Format.printf "reachable: ";
  Vertex.print_set reachable_vertexes;
  Format.printf "deleted: ";
  Vertex.print_set deleted;
  Format.printf "%!";
  Format.printf "cycles: %s%!"
    (String.concat "\n"
       (List.map show (simple_cycles graph multiparent remaining)));
  let reachable_trees v ts = reachable graph multiparent v :: ts in
  ( reachable_tree,
    Vertex.Set.fold reachable_trees multiparent [],
    Vertex.Set.fold reachable_trees deleted [],
    simple_cycles graph multiparent remaining )
