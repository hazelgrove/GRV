open Type
module Let_syntax = Error.Let_syntax

let fail = Error.fail

let return = Error.return

(* TODO: put in Type.ml? *)
let compatible (origin : string) (vertex : Vertex.t)
    (index : Lang.Index.t option) (expected : Type.t) (actual : Type.t) :
    unit Error.t =
  let rec go expected actual =
    match (expected, actual) with
    | Unknown, _ -> return ()
    | _, Unknown -> return ()
    | Num, Num -> return ()
    | Arrow (expected1, expected2), Arrow (actual1, actual2) ->
        let%bind () = go expected1 actual1 and () = go expected2 actual2 in
        return ()
    | _, _ ->
        fail origin vertex index "expected type %s; actual type %s"
          (show expected) (show actual)
  in
  go actual expected

type type_env = Type.t Env.t

(* TODO: analyze against vertex instead of type? *)

(* let rec consistent_cursor (graph : Grapht.t) (cursor1 : Cursor.t) (cursor2 : Cursor.t) *)

(* TODO: Print out final type or None *)
(* Free var, type inconsistency, sort errors(?), conflicts *)

let match_fail (loc : string) (sort : Lang.Sort.t) (vertex : Vertex.t) : 'a =
  let c = vertex.value in
  if Lang.Constructor.sort_of c = sort then
    failwith
      (Printf.sprintf "%s: Missing case for %s in %s" loc
         (Lang.Constructor.show c) (Vertex.show vertex))
  else
    failwith (Printf.sprintf "%s: Non-Typ vertex %s" loc (Vertex.show vertex))

(**************)
(**** Type ****)
(**************)

let rec eval_typ_cursor (graph : Graph.t) (cursor : Cursor.t) : Type.t Error.t =
  match Edge.Set.elements (Graph.cursor_children graph cursor) with
  | [] -> return Unknown
  | [ edge ] -> eval_typ_vertex graph (Edge.target edge)
  | _edges ->
      (* Recur anyway? (* Recur anyway? no *)  Could check if all synth to same type? *)
      (* Technically indeterminate / a type conflict *)
      return Unknown

and eval_typ_vertex (graph : Graph.t) (vertex : Vertex.t) : Type.t Error.t =
  match vertex.value with
  | Typ_num -> return Num
  | Typ_arrow ->
      let%bind t_arg = eval_typ_cursor graph (Cursor.mk vertex Typ_arrow_arg)
      and t_result =
        eval_typ_cursor graph (Cursor.mk vertex Typ_arrow_result)
      in
      return (Arrow (t_arg, t_result))
  | _ -> match_fail __LOC__ Lang.Sort.Typ vertex

(*************)
(**** Pat ****)
(*************)

(* Note that we add all bindings if there is a conflict. Also, we are not
   checking for shadowing between conflicting bindings. *)
let rec ana_pat_cursor (graph : Graph.t) (env : type_env) (cursor : Cursor.t)
    (typ : Type.t) : type_env Error.t =
  let edges = Edge.Set.elements (Graph.cursor_children graph cursor) in
  List.fold_left
    (fun env edge ->
      let%bind env = env in
      ana_pat_vertex graph env (Edge.target edge) typ)
    (return env) edges

and ana_pat_vertex (_graph : Graph.t) (env : type_env) (vertex : Vertex.t)
    (typ : Type.t) : type_env Error.t =
  match vertex.value with
  | Pat_var string -> return (Env.add string typ env)
  | _ -> match_fail __LOC__ Lang.Sort.Pat vertex

(*************)
(**** Exp ****)
(*************)

(* TODO: vertexes to stop at *)

let rec syn_exp_vertex (graph : Graph.t) (env : type_env) (vertex : Vertex.t) :
    Type.t Error.t =
  match vertex.value with
  | Exp_var string -> (
      match Env.find_opt string env with
      | None -> fail __LOC__ vertex None "unbound variable: %s" string
      | Some t -> return t )
  | Exp_lam ->
      let%bind typ =
        eval_typ_cursor graph (Cursor.mk vertex Exp_lam_param_type)
      in
      let%bind env' =
        ana_pat_cursor graph env (Cursor.mk vertex Exp_lam_param) typ
      in
      let%bind body =
        syn_exp_cursor graph env' (Cursor.mk vertex Exp_lam_body)
      in
      return (Arrow (typ, body))
  | Exp_app ->
      let%bind func = syn_exp_cursor graph env (Cursor.mk vertex Exp_app_fun) in
      let%bind param_type, result_type =
        match func with
        | Arrow (param_type, result_type) -> return (param_type, result_type)
        | Unknown -> return (Unknown, Unknown)
        | Num ->
            fail __LOC__ vertex None "expected a function type; actual type %s"
              (Type.show func)
      in
      let%bind () =
        ana_exp_cursor graph env (Cursor.mk vertex Exp_app_arg) param_type
      in
      return result_type
  | Exp_num _ -> return Num
  | Exp_plus ->
      let%bind () =
        ana_exp_cursor graph env (Cursor.mk vertex Exp_plus_left) Num
      and () = ana_exp_cursor graph env (Cursor.mk vertex Exp_plus_right) Num in
      return Num
  | _ -> match_fail __LOC__ Lang.Sort.Exp vertex

and ana_exp_vertex (graph : Graph.t) (env : type_env) (vertex : Vertex.t)
    (typ : Type.t) : unit Error.t =
  (* TODO: When would we not just call syn? *)
  let%bind typ' = syn_exp_vertex graph env vertex in
  compatible __LOC__ vertex None typ typ'

and syn_exp_cursor (graph : Graph.t) (env : type_env) (cursor : Cursor.t) :
    Type.t Error.t =
  match Edge.Set.elements (Graph.cursor_children graph cursor) with
  | [] -> return Unknown
  | [ edge ] -> syn_exp_vertex graph env (Edge.target edge)
  | _edges ->
      (* TODO: join type? *)
      return Unknown

and ana_exp_cursor (graph : Graph.t) (env : type_env) (cursor : Cursor.t)
    (typ : Type.t) : unit Error.t =
  let edges = Edge.Set.elements (Graph.cursor_children graph cursor) in
  let%bind () =
    List.fold_left
      (fun previous_result edge ->
        let%bind () = previous_result
        and () = ana_exp_vertex graph env (Edge.target edge) typ in
        return ())
      (return ()) edges
  in
  return ()

let syn_root_vertex (graph : Graph.t)
    (* TODO:? (env : type_env) *) (vertex : Vertex.t) : Type.t Error.t =
  match vertex.value with
  | Root_root ->
      syn_exp_cursor graph Env.empty (Cursor.mk vertex Root_root_root)
  | _ -> match_fail __LOC__ Lang.Sort.Root vertex

let syn_root_cursor (graph : Graph.t) (cursor : Cursor.t) : Type.t Error.t =
  match Edge.Set.elements (Graph.cursor_children graph cursor) with
  | [] -> return Unknown
  | [ edge ] -> syn_root_vertex graph (Edge.target edge)
  | _edges ->
      (* TODO: join type? *)
      return Unknown

(* TODO: ana_root ? *)
