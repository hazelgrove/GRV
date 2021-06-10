open OptionUtil.Syntax

type t = EdgeState.t Edge.Map.t

type binding = Edge.t * EdgeState.t

let edges (graph : t) : Edge.Set.t = graph |> Edge.Map.keys

let invertex (graph : t) : Vertex.t option =
  let edges = edges graph in
  let targets = Edge.Set.targets edges in
  let* vertex = Vertex.Set.choose_opt targets in
  match Vertex.Set.is_empty (Vertex.Set.remove vertex targets) with
  | true -> Some vertex
  | false -> None

let singleton (edge : Edge.t) (state : EdgeState.t) : t =
  Edge.Map.singleton edge state

let of_list (bindings : binding list) : t = Edge.Map.of_list bindings

let filter (f : Edge.t -> EdgeState.t -> bool) (graph : t) : t =
  Edge.Map.filter f graph

let outedges (vertex : Vertex.t) (position : GroveLang.position) (graph : t) :
    Edge.Set.t =
  graph
  |> filter (fun edge state ->
         edge.source = vertex && edge.position = position && state = Plus)
  |> Edge.Map.keys
