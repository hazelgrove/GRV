module Lang = GroveLang;
module Vertex = Vertex.Make(Lang);
module Edge = Edge.Make(Lang, Vertex);
module Graph = Graph.Make(Lang, Vertex, Edge);
module Graph_action = Graph_action.Make(Lang, Vertex, Edge);
module PositionMap = PositionMap.Make(Lang);
module Term = Term.Make(Lang, Vertex, Edge, Graph, PositionMap);
module Cursor: Cursor.T = Cursor.Make(Lang, Vertex);
