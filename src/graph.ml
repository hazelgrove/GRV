module Uuid = struct
  type t = int
end

(* type vertex_label

type 'ctor vertex_id

type 'ctor 'sort ctor_sort =
| CSApp : App Expr ctor_sort
| CSLam : Lam Expr ctor_sort

type 'ctor 'index 'sort child_sort =
| CSApp1 : App Fun Exp child_sort

type 'src_ctor 'child_index 'txt_ctor edge_id
| App : Uuid.t -> 'tgt_ctor 'tgt_sort ctor_sort -> 'src_ctor 'child_index 'tgt_sort child -> 'src_ctor 'child_index 'tgt_ctor edge_id *)

(* Sorts *)
type exp

type typ

(* Constructors *)
type exp_app

type typ_app

(* Constructor sorts *)
type ('ctor, 'sort) ctor_sort =
  | ExpApp : (exp_app, exp) ctor_sort
  | TypApp : (typ_app, typ) ctor_sort

(*TODO*)
type 'ctor ctor_vertex = Vertex : 'ctor ctor_vertex

and 'sort sort_vertex =
  (* Note that this packs up 'ctor as an existential *)
  | V : ('ctor, 'sort) ctor_sort * 'ctor ctor_vertex -> 'sort sort_vertex

(* Indexes *)
type ('parent_ctor, 'child_sort) index =
  | ExpAppFun : (exp_app, exp) index
  | ExpAppArg : (exp_app, exp) index

let get : ('ctor, 'sort) index -> 'ctor ctor_vertex -> 'sort sort_vertex =
  failwith __LOC__

(* Example of traversal *)
let rec check_exp (x : exp sort_vertex) : unit =
  match x with
  | V (ExpApp, (x : exp_app ctor_vertex)) ->
      let f : exp sort_vertex = get ExpAppFun x in
      check_exp f

and check_typ (x : typ sort_vertex) : unit =
  match x with V (TypApp, (_x : typ_app ctor_vertex)) -> failwith __LOC__

(*


type var
type exp
type typ

type 'src 'tgt edge_info =
| LamTyp : 'src 'tgt edge_info

type 'a list =
| IntCons : int -> 'a list -> int list
| StringCons : 'a -> 'a list -> string list
| Nil : 'a list

let f (x: int list) =
  match x with
  | IntCons =>
  | NilCons => 
  end

let v1 : exp vertex = { ...}
let v2 : typ vertex = {...}
let e : exp typ edge = ....

val target : src tgt edge -> tgt vertex = ...;


(**
  Lam: var typ expr
 *)

type constructor =
  | Lam
  | Apply
  | Var;;

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
