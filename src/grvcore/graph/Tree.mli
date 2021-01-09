(** A tree representation for disjoint graph components. *)

(** A tree is either a vertex with a (possibly empty) set of [children] or a
    reference to one. *)
type t = Vertex of Vertex.t * children Position_map.t | Ref of Vertex.t

and children = child list

(** A child is a tree paired with its parent edge id. *)
and child = { edge_id : Uuid.Id.t; tree : t }

(** {1 Constructors} *)

val child : Uuid.Id.t -> t -> child

val child_map :
  (Lang.Position.t * (Uuid.Id.t * t) list) list -> children Position_map.t

val vertex : Vertex.t -> (Lang.Position.t * (Uuid.Id.t * t) list) list -> t

(** {1 String Conversions} *)

val to_string : t -> string
