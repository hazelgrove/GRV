type env = Type.t Env.t

(* TODO: analyze against vertex instead of type? *)

(* TODO: Cycles via Graph meta-data *)

let fail (loc : string) (sort : Lang.Sort.t) (vertex : Vertex.t) : 'a =
  let c = vertex.value in
  if Lang.Constructor.sort_of c = sort then
    failwith
      (Printf.sprintf "%s: Missing case for %s in %s" loc
         (Lang.Constructor.show c) (Vertex.show vertex))
  else
    failwith (Printf.sprintf "%s: Non-Typ vertex %s" loc (Vertex.show vertex))

(* let rec consistent_cursor (graph : Grapht.t) (cursor1 : Cursor.t) (cursor2 : Cursor.t) *)

let rec syn_typ_cursor (graph : Graph.t) (cursor : Cursor.t) : Type.t =
  match Edge.Set.elements (Graph.children cursor graph) with
  | [] -> Unknown
  | [ edge ] -> syn_typ_vertex graph edge.target
  | _edges ->
      (* Recur anyway?  Could check if all synth to same type? *) Unknown

and syn_typ_vertex (graph : Graph.t) (vertex : Vertex.t) : Type.t =
  match vertex.value with
  | Typ_num -> Num
  | Typ_arrow ->
      let t_arg = syn_typ_cursor graph (Cursor.mk vertex Typ_arrow_arg)
      and t_result = syn_typ_cursor graph (Cursor.mk vertex Typ_arrow_result) in
      Arrow (t_arg, t_result)
  | _ -> fail __LOC__ Lang.Sort.Typ vertex

let ann_pat_cursor (_graph : Graph.t) (_env : env) (_pat_cursor : Cursor.t)
    (_typ : Type.t) : env =
  failwith __LOC__

let rec syn_exp_cursor (graph : Graph.t) (env : env) (cursor : Cursor.t) :
    Type.t =
  match Edge.Set.elements (Graph.children cursor graph) with
  | [] -> Unknown
  | [ edge ] -> syn_exp_vertex graph env edge.target
  | _edges -> (*TODO*) Unknown

and ann_exp_cursor (graph : Graph.t) (env : env) (cursor : Cursor.t)
    (typ : Type.t) : bool =
  match Edge.Set.elements (Graph.children cursor graph) with
  | [] -> false
  | [ edge ] -> ann_exp_vertex graph env edge.target typ
  | _edges -> (*TODO*) false

and syn_exp_vertex (graph : Graph.t) (env : env) (vertex : Vertex.t) : Type.t =
  match vertex.value with
  | Exp_var string -> (
      match Hashtbl.find_opt env string with None -> Unknown | Some t -> t )
  | Exp_lam ->
      let typ = syn_typ_cursor graph (Cursor.mk vertex Exp_lam_param_type) in
      let env' =
        ann_pat_cursor graph env (Cursor.mk vertex Exp_lam_param) typ
      in
      let body = syn_exp_cursor graph env' (Cursor.mk vertex Exp_lam_body) in
      Arrow (typ, body)
  | Exp_app -> (
      let func = syn_exp_cursor graph env (Cursor.mk vertex Exp_app_fun) in
      match func with
      | Arrow (param_type, result_type) ->
          if ann_exp_cursor graph env (Cursor.mk vertex Exp_app_arg) param_type
          then result_type
          else Unknown
      | _ -> Unknown )
  | Exp_num _ -> Num
  | Exp_plus ->
      let (*TODO*) _left =
        ann_exp_cursor graph env (Cursor.mk vertex Exp_plus_left) Num
      in
      let (*TODO*) _right =
        ann_exp_cursor graph env (Cursor.mk vertex Exp_plus_right) Num
      in
      (* TODO*) Num
  | _ -> fail __LOC__ Lang.Sort.Exp vertex

and ann_exp_vertex (graph : Graph.t) (env : env) (vertex : Vertex.t)
    (typ : Type.t) : bool =
  match vertex.value with
  | Exp_var string -> (
      match Hashtbl.find_opt env string with
      | None -> (* TODO: what if typ is Unknown *) false
      | Some s -> typ = s )
  | Exp_lam -> (
      let param_type =
        syn_typ_cursor graph (Cursor.mk vertex Exp_lam_param_type)
      in
      let _env' =
        ann_pat_cursor graph env (Cursor.mk vertex Exp_lam_param) param_type
      in
      match typ with
      | Unknown -> failwith __LOC__
      | Arrow (arg_type, result_type) ->
          param_type = arg_type
          && ann_exp_cursor graph env
               (Cursor.mk vertex Exp_lam_body)
               result_type
      | _ -> failwith __LOC__ )
  | Exp_app -> (
      let func = syn_exp_cursor graph env (Cursor.mk vertex Exp_app_fun) in
      match func with
      | Unknown -> failwith __LOC__
      | Arrow (param_type, result_type) ->
          let arg =
            ann_exp_cursor graph env (Cursor.mk vertex Exp_app_arg) param_type
          in
          arg && typ = result_type
      | _ -> failwith __LOC__ )
  | Exp_num _ -> typ = Num
  | Exp_plus ->
      let left =
        ann_exp_cursor graph env (Cursor.mk vertex Exp_plus_left) Num
      in
      let right =
        ann_exp_cursor graph env (Cursor.mk vertex Exp_plus_right) Num
      in
      (* TODO*) left && right && typ = Num
  | _ -> fail __LOC__ Lang.Sort.Exp vertex

(* let syn_typ
let syn_pat
let syn_exp
let syn_root *)
