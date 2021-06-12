open OptionUtil.Syntax
include Edge.Map

type nonrec t = EdgeState.t t

type binding = Edge.t * EdgeState.t

let edges : t -> Edge.Set.t = Edge.Map.keys

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

let outedges (vertex : Vertex.t) (position : GroveLang.position) (graph : t) :
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
