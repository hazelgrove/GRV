open OptionUtil.Syntax

type t = { invertex : Vertex.t; graph : Graph.t }

let singleton (edge : Edge.t) (state : EdgeState.t) : t =
  let invertex = edge.target in
  let graph = Graph.singleton edge state in
  { invertex; graph }

let of_list (bindings : Graph.binding list) : t option =
  let graph = Graph.of_list bindings in
  let+ invertex = Graph.invertex graph in
  { invertex; graph }

let of_vertex (vertex : Vertex.t) (graph : Graph.t) : t option =
  let graph =
    Graph.filter (fun (edge : Edge.t) _ -> vertex = edge.target) graph
  in
  let+ invertex = Graph.invertex graph in
  { invertex; graph }
