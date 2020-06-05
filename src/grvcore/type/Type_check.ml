type type_env = Type.t Env.t

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

type error = Error of Vertex.t * Lang.Index.t option * string

type syn = Success of Type.t | Error of error list

type ana = Success | Error of error list

(* let rec consistent_cursor (graph : Grapht.t) (cursor1 : Cursor.t) (cursor2 : Cursor.t) *)

(* TODO: Print out final type or None *)
(* Free var, type inconsistency, sort errors(?), conflicts *)
(* Return vertex set of where incosistencies *)

(**************)
(**** Type ****)
(**************)

let rec eval_typ_cursor (graph : Graph.t) (cursor : Cursor.t) : Type.t =
  match Edge.Set.elements (Graph.children graph cursor) with
  | [] -> Unknown
  | [ edge ] -> eval_typ_vertex graph (Edge.target edge)
  | _edges ->
      (* Recur anyway? (* Recur anyway? no *)  Could check if all synth to same type? *)
      (* Technically indeterminate / a type conflict *)
      Unknown

and eval_typ_vertex (graph : Graph.t) (vertex : Vertex.t) : Type.t =
  match vertex.value with
  | Typ_num -> Num
  | Typ_arrow ->
      let t_arg = eval_typ_cursor graph (Cursor.mk vertex Typ_arrow_arg)
      and t_result =
        eval_typ_cursor graph (Cursor.mk vertex Typ_arrow_result)
      in
      Arrow (t_arg, t_result)
  | _ -> fail __LOC__ Lang.Sort.Typ vertex

(*************)
(**** Pat ****)
(*************)

let rec ana_pat_cursor (graph : Graph.t) (env : type_env) (cursor : Cursor.t)
    (typ : Type.t) : type_env =
  let edges = Edge.Set.elements (Graph.children graph cursor) in
  (* Note that we add all bindings if there is a conflict.
     Also, we are not checking for shadowing between conflicting bindings. *)
  List.fold_left
    (fun env edge -> ana_pat_vertex graph env (Edge.target edge) typ)
    env edges

(* TODO: what if conflict? add all. *)
and ana_pat_vertex (_graph : Graph.t) (env : type_env) (vertex : Vertex.t)
    (typ : Type.t) : type_env =
  match vertex.value with
  | Pat_var string -> Env.add string typ env
  | _ -> fail __LOC__ Lang.Sort.Pat vertex

(*************)
(**** Exp ****)
(*************)

let rec syn_exp_cursor (graph : Graph.t) (env : type_env) (cursor : Cursor.t) :
    Type.t =
  match Edge.Set.elements (Graph.children graph cursor) with
  | [] -> Unknown
  | [ edge ] -> syn_exp_vertex graph env (Edge.target edge)
  | _edges ->
      (* (* Could check if all synth to same type? *) TODO: join type? *)
      Unknown

and ana_exp_cursor (graph : Graph.t) (env : type_env) (cursor : Cursor.t)
    (typ : Type.t) : bool =
  match Edge.Set.elements (Graph.children graph cursor) with
  | [] -> false
  | [ edge ] -> ana_exp_vertex graph env (Edge.target edge) typ
  | _edges -> (*TODO*) false

and syn_exp_vertex (graph : Graph.t) (env : type_env) (vertex : Vertex.t) :
    Type.t (* TODO: option*) =
  match vertex.value with
  | Exp_var string -> (
      match Env.find_opt string env with None -> Unknown | Some t -> t )
  | Exp_lam ->
      let typ = eval_typ_cursor graph (Cursor.mk vertex Exp_lam_param_type) in
      let env' =
        ana_pat_cursor graph env (Cursor.mk vertex Exp_lam_param) typ
      in
      let body = syn_exp_cursor graph env' (Cursor.mk vertex Exp_lam_body) in
      Arrow (typ, body)
  | Exp_app -> (
      let func = syn_exp_cursor graph env (Cursor.mk vertex Exp_app_fun) in
      match func with
      (* Matched arrow type *)
      | Arrow (param_type, result_type) ->
          if ana_exp_cursor graph env (Cursor.mk vertex Exp_app_arg) param_type
          then result_type
          else failwith __LOC__ (* TODO None *)
      | Unknown -> failwith __LOC__ (* TODO: treat as Unknown -> Unknown*)
      | Num -> Unknown )
  | Exp_num _ -> Num
  | Exp_plus ->
      let (*TODO*) _left =
        ana_exp_cursor graph env (Cursor.mk vertex Exp_plus_left) Num
      in
      let (*TODO*) _right =
        ana_exp_cursor graph env (Cursor.mk vertex Exp_plus_right) Num
      in
      (* TODO*) Num
  | _ -> fail __LOC__ Lang.Sort.Exp vertex

and ana_exp_vertex (graph : Graph.t) (env : type_env) (vertex : Vertex.t)
    (typ : Type.t) : bool =
  (* TODO: check type consistency after defering to synthesis *)
  match vertex.value with
  | Exp_var string -> (
      match Env.find_opt string env with
      | None -> (* TODO: what if typ is Unknown *) false
      | Some s -> typ = s )
  | Exp_lam -> (
      let param_type =
        eval_typ_cursor graph (Cursor.mk vertex Exp_lam_param_type)
      in
      let _env' =
        ana_pat_cursor graph env (Cursor.mk vertex Exp_lam_param) param_type
      in
      match typ with
      | Unknown -> failwith __LOC__
      | Arrow (arg_type, result_type) ->
          param_type = arg_type
          && ana_exp_cursor graph env
               (Cursor.mk vertex Exp_lam_body)
               result_type
      | _ -> failwith __LOC__ )
  | Exp_app -> (
      let func = syn_exp_cursor graph env (Cursor.mk vertex Exp_app_fun) in
      match func with
      | Unknown -> failwith __LOC__
      | Arrow (param_type, result_type) ->
          let arg =
            ana_exp_cursor graph env (Cursor.mk vertex Exp_app_arg) param_type
          in
          arg && typ = result_type
      | _ -> failwith __LOC__ )
  | Exp_num _ -> typ = Num
  | Exp_plus ->
      let left =
        ana_exp_cursor graph env (Cursor.mk vertex Exp_plus_left) Num
      in
      let right =
        ana_exp_cursor graph env (Cursor.mk vertex Exp_plus_right) Num
      in
      (* TODO*) left && right && typ = Num
  | _ -> fail __LOC__ Lang.Sort.Exp vertex

(* TODO: syn_root *)
