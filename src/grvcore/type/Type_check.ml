open Type
module Let_syntax = Error.Let_syntax

let error = Error.error

let return = Error.return

type type_env = Type.t Env.t

(* TODO: get types for everywhere via a Map *)
(* TODO: show types at cursor, see Hazel's "Cursor Info" *)

let consistent (origin : string) (vertex : Vertex.t) (expected : Type.t)
    (actual : Type.t) : unit Error.t =
  let rec go expected actual =
    match (expected, actual) with
    | Unknown, _ -> return ()
    | _, Unknown -> return ()
    | Num, Num -> return ()
    | Arrow (expected1, expected2), Arrow (actual1, actual2) ->
        let%bind () = go expected1 actual1 and () = go expected2 actual2 in
        return ()
    | _, _ ->
        error origin vertex "expected type %s; actual type %s" (show expected)
          (show actual)
  in
  go actual expected

let maybe_unknown (ts : t list) : t = match ts with [ t ] -> t | _ -> Unknown

(* TODO: analyze against vertex instead of type? *)

(* let rec consistent_cursor (graph : Grapht.t) (cursor1 : Cursor.t) (cursor2 : Cursor.t) *)

let match_fail (loc : string) (sort : Lang.Sort.t) (vertex : Vertex.t) : 'a =
  if Lang.Constructor.sort_of vertex.value = sort then
    failwith
      (Printf.sprintf "%s: Missing case for %s in %s" loc
         (Lang.Constructor.show vertex.value)
         (Vertex.show vertex))
  else
    failwith (Printf.sprintf "%s: Non-Typ vertex %s" loc (Vertex.show vertex))

(**************)
(**** Type ****)
(**************)

(* TODO: change 'eval' to 'expand' (or other better name?) *)
let rec eval_typ_vertex (graph : Graph.t) (vertex : Vertex.t) : Type.t Error.t =
  match vertex.value with
  | Typ_num -> return Num
  | Typ_arrow ->
      let%bind t_arg = eval_typ_cursor graph (Cursor.mk vertex Typ_arrow_arg)
      and t_result =
        eval_typ_cursor graph (Cursor.mk vertex Typ_arrow_result)
      in
      return (Arrow (t_arg, t_result))
  | _ -> match_fail __LOC__ Lang.Sort.Typ vertex

and eval_typ_cursor (graph : Graph.t) (cursor : Cursor.t) : Type.t Error.t =
  let edges = Edge.Set.elements (Graph.cursor_children graph cursor) in
  let go edge = eval_typ_vertex graph (Edge.target edge) in
  let%bind ts = Error.sequence (List.map go edges) in
  return (maybe_unknown ts)

(*************)
(**** Pat ****)
(*************)

(* TODO: syn_pat_vertex *)

(* Note that we add all bindings if there is a conflict. Also, we are not
   checking for shadowing between conflicting bindings. *)

let rec ana_pat_vertex (_graph : Graph.t) (env : type_env) (vertex : Vertex.t)
    (typ : Type.t) : type_env Error.t =
  match vertex.value with
  | Pat_var string -> return (Env.add string typ env)
  | _ -> match_fail __LOC__ Lang.Sort.Pat vertex

and ana_pat_cursor (graph : Graph.t) (env : type_env) (cursor : Cursor.t)
    (typ : Type.t) : type_env Error.t =
  let edges = Edge.Set.elements (Graph.cursor_children graph cursor) in
  List.fold_left
    (fun env edge ->
      let%bind env = env in
      ana_pat_vertex graph env (Edge.target edge) typ)
    (return env) edges

(*************)
(**** Exp ****)
(*************)

(* TODO: vertexes to stop at *)
(* TODO: type map *)
(* TODO: error map *)

let rec syn_exp_vertex (graph : Graph.t) (env : type_env) (vertex : Vertex.t) :
    Type.t Error.t =
  match vertex.value with
  | Exp_var string -> (
      match Env.find_opt string env with
      | None -> error __LOC__ vertex "unbound variable: %s" string
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
        | Unknown -> return (Unknown, Unknown)
        | Arrow (param_type, result_type) -> return (param_type, result_type)
        | _ ->
            error __LOC__ vertex "expected a function type; actual type %s"
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
  match vertex.value with
  | Exp_cons ->
      let%bind content =
        match typ with
        | Unknown -> return Unknown
        | List t -> return t
        | _ ->
            error __LOC__ vertex "expected a list type; actual type %s"
              (Type.show typ)
      in
      let%bind () =
        ana_exp_cursor graph env (Cursor.mk vertex Exp_cons_head) content
      in
      return ()
  | _ ->
      (* TODO: When would we not just call syn? *)
      (* TODO: Need unannotated lambda or left or right injection (base case) or lists (base case) or tuples (no base case) *)
      let%bind typ' = syn_exp_vertex graph env vertex in
      consistent __LOC__ vertex typ typ'

and syn_exp_cursor (graph : Graph.t) (env : type_env) (cursor : Cursor.t) :
    Type.t Error.t =
  let edges = Edge.Set.elements (Graph.cursor_children graph cursor) in
  let go edge = syn_exp_vertex graph env (Edge.target edge) in
  let%bind ts = Error.sequence (List.map go edges) in
  return (maybe_unknown ts)

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

let syn_root_vertex (graph : Graph.t) (env : type_env) (vertex : Vertex.t) :
    Type.t Error.t =
  match vertex.value with
  | Root_root -> syn_exp_cursor graph env (Cursor.mk vertex Root_root_root)
  | _ -> match_fail __LOC__ Lang.Sort.Root vertex

let syn_root_cursor (graph : Graph.t) (env : type_env) (cursor : Cursor.t) :
    Type.t Error.t =
  let edges = Edge.Set.elements (Graph.cursor_children graph cursor) in
  let go edge = syn_root_vertex graph env (Edge.target edge) in
  let%bind ts = Error.sequence (List.map go edges) in
  return (maybe_unknown ts)

(* TODO: Implement ana_root *)
