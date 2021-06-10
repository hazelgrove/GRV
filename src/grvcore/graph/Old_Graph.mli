(** Simple multigraphs with a total ordering on vertices and edges. *)

type t = Old_Edge_state.t Old_Edge.Map.t
(** A graph maps each edge it contains to the current state of the edge in the
    graph. Edges that have not yet been {{!Old_Edge_state.Created} [Created]} or
    {{!Old_Edge_state.Deleted} [Deleted]} are implicitly mapped to ⊥. *)

val empty : t
(** The empty graph. *)

val add : Old_Edge.t -> Old_Edge_state.t -> t -> t

(** {1 Graph Actions} *)

val apply_action : t -> Graph_action.t -> t
(** [apply_action g a] returns a graph containing the same edges and edge states
    as [g], except for [e] where [e] is the edge referenced by [a], which
    acquires the greater of its two states in [a] and [g]. *)

(** {1 Edge Queries} *)

val edges : t -> Old_Edge.Set.t
(** Returns the set of all {{!Old_Edge_state.Created} [Created]} or
    {{!Old_Edge_state.Deleted} [Deleted]} edges of the given graph. *)

val live_edges : t -> Old_Edge.Set.t
(** Returns the set of all {{!Old_Edge_state.Created} [Created]} edges of the given
    graph. *)

val parent_edges : t -> Old_Vertex.t -> Old_Edge.Set.t
(** [parent_edges g v] returns a set containing all edges of [g] with
    destination [v]. *)

val child_edges : t -> Old_Vertex.t -> Lang.Position.t -> Old_Edge.Set.t
(** [child_edges g v p] returns a set containing all live edges of [g] that
    originate from vertex [v] at position [p]. *)

(** {1 Vertex Queries} *)

val vertexes : t -> Old_Vertex.Set.t
(** Returns the set of all vertices used in the edges of the given graph. *)

val live_vertexes : t -> Old_Vertex.Set.t
(** Returns the set of all vertices used in the live edges of the given graph.
  *)

val parent_vertexes : t -> Old_Vertex.t -> Old_Vertex.Set.t
(** [parent_vertexes g v] returns a set containing the originating vertices of
    any edges in [g] that have destination [v]. *)

val deleted : t -> Old_Vertex.Set.t
(** Returns the set of all non-root vertices of the given graph that are not the
    destination of any live edges. *)

val multiparented : t -> Old_Vertex.Set.t
(** Returns the set of all vertices of the given graph that are the destination
    of multiple live edges. *)

val vertex : t -> Uuid.Id.t -> Old_Vertex.t option
(** [vertex g id] returns [Some v] if [g] identifies [v] with [id]. *)

(** {1 S-Expression Conversions} *)

val sexp_of_t : t -> Sexplib.Sexp.t

val t_of_sexp : Sexplib.Sexp.t -> t
