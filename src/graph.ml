module Uuid = struct
  type t = int

  (* let set_seed : int -> int -> unit = failwith __LOC__ *)
end

(*

We use GADTs to encode the following types at the type level.

TODO: write a PPX that does this encoding for us (a useful template might be https://github.com/Astrocoders/lenses-ppx)

module%type_encode Sorts = struct
  type root =
  | Root of exp

  type id =
  | Id of string[@not_encoded]

  type exp =
  | Lam of id[@name param] * typ[@name param_type] * exp[@body]
  | App of exp * exp
  | Var of id

  type typ =
  | App of typ typ
  | Var of id
end
*)

(* TODO: is where a way to modularize these? Exhaustiveness checks failed when I
tried this. *)

type root (* TODO?: = Root *)

type root_root = Root_root

type id (* TODO?: = Id *)

type id_id = Id_id of string

type exp (* TODO?: = Exp *)

type exp_lam = Exp_lam

type exp_app = Exp_app

type exp_var = Exp_var

type typ (* TODO?: = Typ *)

type typ_app = Typ_app

type typ_var = Typ_var

(* TODO: some way to "kind-check" uses of ctor versus sort *)

(* Connecting constructors to sorts *)
(* TODO: maybe one constructor per sort and have type in sort to further specifics *)
type ('ctor, 'sort) sort_of_ctor =
  | Root_root : (root_root, root) sort_of_ctor
  | Id_id : (id_id, id) sort_of_ctor
  | Exp_lam : (exp_lam, exp) sort_of_ctor
  | Exp_app : (exp_app, exp) sort_of_ctor
  | Exp_var : (exp_var, exp) sort_of_ctor
  | Typ_app : (typ_app, typ) sort_of_ctor
  | Typ_var : (typ_var, typ) sort_of_ctor

(* Connecting constructors to the sorts of their children *)
(* NOTE: this also serves as the `index` of child edges *)
type ('ctor, 'sort) sort_of_child =
  | Root_root_root : (root_root, root) sort_of_child
  (* id_id has no children *)
  | Exp_lam_param : (exp_lam, id) sort_of_child
  | Exp_lam_param_type : (exp_lam, typ) sort_of_child
  | Exp_lam_body : (exp_lam, exp) sort_of_child
  | Exp_app_fun : (exp_app, exp) sort_of_child
  | Exp_app_arg : (exp_app, exp) sort_of_child
  | Exp_var_id : (exp_var, id) sort_of_child
  (* TODO: more Exp *)
  | Typ_app_fun : (typ_app, typ) sort_of_child

(**************************** *)

(* Vertex with constructor type *)
type 'ctor ctor_vertex = Ctor_vertex : Uuid.t * 'ctor -> 'ctor ctor_vertex

(* Vertex with sort type *)
type 'sort sort_vertex =
  (* Note that this packs up 'ctor as an existential *)
  | Sort_vertex :
      ('ctor, 'sort) sort_of_ctor * 'ctor ctor_vertex
      -> 'sort sort_vertex

(* Edge *)
type 'target_sort edge =
  | Edge : {
      id : Uuid.t;
      source : 'source_ctor ctor_vertex;
      index : ('source_ctor, 'target_sort) sort_of_child;
      target : 'target_sort sort_vertex;
    }
      -> 'target_sort edge

let edges_from :
      'ctor 'sort. 'ctor ctor_vertex -> ('ctor, 'sort) sort_of_child ->
      'sort edge list =
 fun _ -> failwith __LOC__

let edges_to : 'ctor 'sort. 'sort sort_vertex -> 'sort edge list =
 fun _ -> failwith __LOC__

let target : 'ctor 'sort. 'sort edge -> 'sort sort_vertex =
 fun (Edge { target; _ }) -> target

(* There is no `source` function to match `target`, since that would require an
existential for the parent's constructor.  Instead, use a pattern match on
`Edge`. *)
(* let source : 'ctor 'sort. 'sort edge -> ('ctor, 'sort) sort_of_child * 'ctor ctor_vertex
    =
 fun _ -> failwith __LOC__ *)

let vertexes_from :
      'ctor 'sort. 'ctor ctor_vertex -> ('ctor, 'sort) sort_of_child ->
      'sort sort_vertex list =
 fun vertex index -> List.map target (edges_from vertex index)

(* Example of traversal *)
let rec check_exp (x : exp sort_vertex) : unit =
  match x with
  | Sort_vertex (Exp_app, (vertex : exp_app ctor_vertex)) ->
      let edge : exp edge list = edges_from vertex Exp_app_fun in
      let f : exp sort_vertex list = List.map target edge in
      List.iter check_exp f
  | _ -> failwith __LOC__

and check_typ (x : typ sort_vertex) : unit =
  match x with
  | Sort_vertex (Typ_app, _) -> failwith __LOC__
  | Sort_vertex (Typ_var, _) -> failwith __LOC__

(* TODO *)
type 'ctor vertex_info =
  | Vertex_info : {
      vertex : 'ctor ctor_vertex;
      parents : 'sort edge list;
          (* TODO: children: ...
             Need something indexed by sort_of_child instead of array *)
    }
      -> 'ctor vertex_info

(* TODO: replace list with set *)

(*

type index = int;;

type vertex_name = {
  id: Uuid.t,
  constructor: constructor,
}

and vertex = {
  name: vertex_name,
  sort: sort,
  parents: edge Set.t,
  children: (edge Set.t) array (* Length based on arity *)
}

and edge_name = {
  id: Uuid.t,
  src: vertex,
  dst: vertex,
  index: index,
}

and edge = {
  name: edge_name;
  alive: bool; (* NOTE: edge objects for uncreated edges do not exist *)
};;

type graph = {
  edges: edge_name bool Map.t;
  vertex_by_name: vertex_name vertex Map.t,
  edge_by_name: edge_name edge Map.t,
};; *)
