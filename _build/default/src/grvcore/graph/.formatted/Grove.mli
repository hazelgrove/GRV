(** Graph decomposition *)

type t = {
  multiparented : Tree.t list;
  deleted : Tree.t list;
  reachable : Tree.t;
  wreaths : Tree.t list;
}
(** A graph can be partitioned into four disjoint sets of [Tree.t]s *)

val traverse_vertex :
  ?seen:Vertex.Set.t ->
  ?remaining:Vertex.Set.t ->
  Vertex.t ->
  Edge.Set.t Vertex.Map.t ->
  Tree.t * Vertex.Set.t * Vertex.Set.t
(** [traverse_vertex vertex children ~seen ~remaining] recursively transforms
    [vertex] into a [Tree.t]. The result is a tree and updated [seen] and
    [remaining] sets with the tree's vertices removed from [remaining] and added
    to [seen]. If [vertex] is already in [seen], the resulting tree is a
    [Tree.Ref]. The [children] map is used for efficient adjacency lookups. *)

val decompose : Graph.t -> t * Edge.Set.t Vertex.Map.t
(** [decompose graph] partitions [graph] into disjoint sets of [Tree.t]s. *)
