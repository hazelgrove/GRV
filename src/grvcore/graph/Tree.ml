let child_vertexes (graph : Graph.t) (v : Vertex.t) : Vertex.Set.t =
  Edge.Set.fold
    (fun e vs -> Vertex.Set.add e.value.target vs)
    (Graph.vertex_children graph v)
    Vertex.Set.empty

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

let rec reachable (graph : Graph.t) (multiparent : Vertex.Set.t)
    (vertex : Vertex.t) : t =
  if Vertex.Set.mem vertex multiparent then Ref vertex.id
  else
    let children =
      Edge.Set.fold
        (fun edge children ->
          let tree = reachable graph multiparent edge.value.target in
          IndexMap.update edge.value.source.index
            (function
              | None -> Some [ tree ] | Some trees -> Some (tree :: trees))
            children)
        (Graph.vertex_children graph vertex)
        IndexMap.empty
    in
    Con (vertex, IndexMap.map List.rev children)

(* Returns option to enable backtracking OR to indicate failure *)
let find_a_cycle_vertex ?(seen = Vertex.Set.empty) (graph : Graph.t)
    (vertex : Vertex.t) : Vertex.t option =
  let parents = Graph.parent_vertexes graph vertex in
  let seen_parents = Vertex.Set.inter parents seen in
  if Vertex.Set.is_empty seen_parents then
    let seen = Vertex.Set.add vertex seen in
    Vertex.Set.find_first_opt (fun v -> Vertex.Set.mem v seen) seen_parents
  else Some (Vertex.Set.choose seen_parents)

(* Returns option to enable backtracking ONLY. Every result is
   Some(set containing vertex). *)
let rec find_all_cycle_vertexes ?(seen = Vertex.Set.empty) (graph : Graph.t)
    (vertex : Vertex.t) : Vertex.Set.t option =
  if Vertex.Set.mem vertex seen then Some seen
  else
    let seen = Vertex.Set.add vertex seen in
    Vertex.Set.fold
      (fun v last ->
        match last with
        | Some _ -> last
        | None -> find_all_cycle_vertexes graph v ~seen)
      (Graph.parent_vertexes graph vertex)
      None

let least_vertex : Vertex.Set.t -> Vertex.t = Vertex.Set.min_elt

let simple_cycle (graph : Graph.t) (multiparent : Vertex.Set.t)
    (vertex : Vertex.t) : t option =
  let%map.Util.Option cycle_vertex = find_a_cycle_vertex graph vertex in
  (* we can always assume the following result will be Some(non-empty set)
     because it always contains at least cycle_vertex *)
  let cycle_vertexes =
    Option.get (find_all_cycle_vertexes graph cycle_vertex)
  in
  let cycle_root = least_vertex cycle_vertexes in
  reachable graph multiparent cycle_root

let rec tree_vertexes : t -> Vertex.t list = function
  | Ref _ -> []
  | Con (vertex, children) ->
      IndexMap.fold
        (fun _ trees vertexes ->
          List.concat
            [ vertexes; vertex :: List.concat (List.map tree_vertexes trees) ])
        children []

let subtract_tree_vertexes (vertexes : Vertex.Set.t) (tree : t) : Vertex.Set.t =
  Vertex.Set.diff vertexes (Vertex.Set.of_list (tree_vertexes tree))

let rec simple_cycles (graph : Graph.t) (multiparent : Vertex.Set.t)
    (remaining : Vertex.Set.t) : t list =
  if Vertex.Set.is_empty remaining then []
  else
    let vertex = Vertex.Set.choose remaining in
    match simple_cycle graph multiparent vertex with
    | None ->
        simple_cycles graph multiparent (Vertex.Set.remove vertex remaining)
    | Some tree ->
        let cycle_trees =
          simple_cycles graph multiparent
            (subtract_tree_vertexes remaining tree)
        in
        tree :: cycle_trees

let decompose (graph : Graph.t) : t * t list * t list * t list =
  let vertexes = Graph.vertexes graph in
  let multiparent = Graph.multiparents graph in
  let orphans = Graph.orphans graph in
  let reachable_trees v ts = reachable graph multiparent v :: ts in
  let remaining = Vertex.Set.(diff vertexes (union multiparent orphans)) in
  ( reachable graph multiparent Vertex.root,
    Vertex.Set.fold reachable_trees multiparent [],
    Vertex.Set.fold reachable_trees orphans [],
    simple_cycles graph multiparent remaining )
