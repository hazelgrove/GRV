open OptionUtil.Syntax
include Edge.Map

type nonrec t = EdgeState.t t

let sexp_of_t = Edge.Map.sexp_of_t EdgeState.sexp_of_t

let t_of_sexp = Edge.Map.t_of_sexp EdgeState.t_of_sexp

type binding = Edge.t * EdgeState.t

let edges : t -> Edge.Set.t = keys

let vertices (graph : t) : Vertex.Set.t =
  graph |> edges |> Edge.Set.elements
  |> List.map (fun (e : Edge.t) -> [ e.source; e.target ])
  |> List.concat |> Vertex.Set.of_list

let find_vertex (vertex_id : Vertex.id) (graph : t) : Vertex.t option =
  graph |> vertices |> Vertex.Set.find_first_opt (fun v -> v.id = vertex_id)

let invertex (graph : t) : Vertex.t option =
  let edges = edges graph in
  let targets = Edge.Set.targets edges in
  let* vertex = Vertex.Set.choose_opt targets in
  match Vertex.Set.is_empty (Vertex.Set.remove vertex targets) with
  | true -> Some vertex
  | false -> None

let filter : (Edge.t -> EdgeState.t -> bool) -> t -> t = filter

let live_edges (graph : t) : Edge.Set.t =
  graph |> filter (fun _ state -> state = Plus) |> edges

let outedges (vertex : Vertex.t) (position : GroveLang.Position.t) (graph : t) :
    Edge.Set.t =
  live_edges graph
  |> Edge.Set.filter (fun edge ->
         edge.source = vertex && edge.position = position)

let parents (vertex : Vertex.t) (graph : t) : Vertex.Set.t =
  let parent_edges =
    live_edges graph |> Edge.Set.filter (fun edge -> edge.target = vertex)
  in
  Edge.Set.fold
    (fun edge vertices -> Vertex.Set.add edge.source vertices)
    parent_edges Vertex.Set.empty

let rec ancestors ?(acc : Vertex.Set.t = Vertex.Set.empty) (vertex : Vertex.t)
    (graph : t) : Vertex.Set.t =
  let parents = parents vertex graph in
  let parents = Vertex.Set.diff parents acc in
  if Vertex.Set.is_empty parents then acc
  else ancestors ~acc:(Vertex.Set.union acc parents) vertex graph

let is_unicycle_root (vertex : Vertex.t) (graph : t) : bool =
  match ancestors vertex graph |> Vertex.Set.min_elt_opt with
  | None -> false
  | Some unicycle_root -> vertex = unicycle_root

type indegree = Zero | One | Many

let root_edges (graph : t) : Edge.t list * Edge.t list * Edge.t list =
  let indegrees =
    bindings graph
    |> List.fold_left
         (fun indegrees ((edge, state) : binding) ->
           match state with
           | Plus ->
               Vertex.Map.update edge.target
                 (function
                   | None -> Some Zero
                   | Some Zero -> Some One
                   | Some (One | Many) -> Some Many)
                 indegrees
           | Minus -> indegrees)
         Vertex.Map.empty
  in
  edges graph |> Edge.Set.elements
  |> List.fold_left
       (fun (noparent_edges, multiparent_edges, unicycle_edges) (edge : Edge.t) ->
         match Vertex.Map.find_opt edge.target indegrees with
         | None -> failwith ("impossible " ^ __LOC__)
         | Some Zero ->
             (edge :: noparent_edges, multiparent_edges, unicycle_edges)
         | Some One ->
             if is_unicycle_root edge.target graph then
               (noparent_edges, multiparent_edges, edge :: unicycle_edges)
             else (noparent_edges, multiparent_edges, unicycle_edges)
         | Some Many ->
             (noparent_edges, edge :: multiparent_edges, unicycle_edges))
       ([], [], [])

let apply_edits (graph_edits : GraphAction.t list) (graph : t) : t =
  let actions, edges = List.split graph_edits in
  let bindings = List.combine edges actions |> List.to_seq in
  add_seq bindings graph
