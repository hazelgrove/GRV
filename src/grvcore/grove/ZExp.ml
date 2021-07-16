open OptionUtil.Syntax

type t =
  | Cursor of Exp.t
  | ZLamP of Ingraph.t * ZPat.t * Typ.t * Exp.t
  | ZLamT of Ingraph.t * Pat.t * ZTyp.t * Exp.t
  | ZLamE of Ingraph.t * Pat.t * Typ.t * t
  | ZAppE1 of Ingraph.t * t * Exp.t
  | ZAppE2 of Ingraph.t * Exp.t * t
  | ZPlusE1 of Ingraph.t * t * Exp.t
  | ZPlusE2 of Ingraph.t * Exp.t * t
  | ZTimesE1 of Ingraph.t * t * Exp.t
  | ZTimesE2 of Ingraph.t * Exp.t * t
  | ZConflict of t * Exp.C.t
[@@deriving sexp]

let rec cursor_position : t -> GroveLang.Position.t option = function
  | Cursor _ -> None
  | ZLamP (_, Cursor _, _, _) -> Some LamParam
  | ZLamT (_, _, Cursor _, _) -> Some LamType
  | ZLamE (_, _, _, Cursor _) -> Some LamBody
  | ZAppE1 (_, Cursor _, _) -> Some AppFun
  | ZAppE2 (_, _, Cursor _) -> Some AppArg
  | ZPlusE1 (_, Cursor _, _) -> Some PlusLeft
  | ZPlusE2 (_, _, Cursor _) -> Some PlusRight
  | ZTimesE1 (_, Cursor _, _) -> Some TimesLeft
  | ZTimesE2 (_, _, Cursor _) -> Some TimesRight
  | ZConflict (Cursor exp, _) ->
      let* ingraph = Exp.ingraph exp in
      Ingraph.inposition ingraph
  | ZLamP (_, zpat, _, _) -> ZPat.cursor_position zpat
  | ZLamT (_, _, ztyp, _) -> ZTyp.cursor_position ztyp
  | ZLamE (_, _, _, zexp)
  | ZAppE1 (_, zexp, _)
  | ZAppE2 (_, _, zexp)
  | ZPlusE1 (_, zexp, _)
  | ZPlusE2 (_, _, zexp)
  | ZTimesE1 (_, zexp, _)
  | ZTimesE2 (_, _, zexp)
  | ZConflict (zexp, _) ->
      cursor_position zexp

let rec cursor_sort : t -> GroveLang.Sort.t = function
  | Cursor _ -> Exp
  | ZLamP (_, zpat, _, _) -> ZPat.cursor_sort zpat
  | ZLamT (_, _, ztyp, _) -> ZTyp.cursor_sort ztyp
  | ZLamE (_, _, _, zexp)
  | ZAppE1 (_, zexp, _)
  | ZAppE2 (_, _, zexp)
  | ZPlusE1 (_, zexp, _)
  | ZPlusE2 (_, _, zexp)
  | ZTimesE1 (_, zexp, _)
  | ZTimesE2 (_, _, zexp)
  | ZConflict (zexp, _) ->
      cursor_sort zexp

let rec erase_cursor : t -> Exp.t = function
  | Cursor e -> e
  | ZLamP (ingraph, zpat, ty, body) ->
      Lam (ingraph, ZPat.erase_cursor zpat, ty, body)
  | ZLamT (ingraph, pat, zty, body) ->
      Lam (ingraph, pat, ZTyp.erase_cursor zty, body)
  | ZLamE (ingraph, pat, ty, zbody) -> Lam (ingraph, pat, ty, erase_cursor zbody)
  | ZAppE1 (ingraph, ze1, e2) -> App (ingraph, erase_cursor ze1, e2)
  | ZAppE2 (ingraph, e1, ze2) -> App (ingraph, e1, erase_cursor ze2)
  | ZPlusE1 (ingraph, ze1, e2) -> Plus (ingraph, erase_cursor ze1, e2)
  | ZPlusE2 (ingraph, e1, ze2) -> Plus (ingraph, e1, erase_cursor ze2)
  | ZTimesE1 (ingraph, ze1, e2) -> Times (ingraph, erase_cursor ze1, e2)
  | ZTimesE2 (ingraph, e1, ze2) -> Times (ingraph, e1, erase_cursor ze2)
  | ZConflict (_, conflict) -> Conflict conflict

let is_root (zexp : t) : bool = zexp |> erase_cursor |> Exp.is_root

let rec recomp : t -> Graph.t * GraphCursor.t = function
  | Cursor exp ->
      let graph = Exp.recomp exp in
      let graph_cursor =
        let open GraphCursor in
        match exp with
        | Var (ingraph, _)
        | Lam (ingraph, _, _, _)
        | App (ingraph, _, _)
        | Num (ingraph, _)
        | Plus (ingraph, _, _)
        | Times (ingraph, _, _) ->
            OnVertex ingraph.invertex.id
        | Multiparent edge | Unicycle edge ->
            OnRef (edge.source.id, edge.position)
        | Conflict conflict -> (
            match Exp.C.choose conflict with
            | Var (ingraph, _)
            | Lam (ingraph, _, _, _)
            | App (ingraph, _, _)
            | Num (ingraph, _)
            | Plus (ingraph, _, _)
            | Times (ingraph, _, _) -> (
                match Ingraph.edges ingraph |> Edge.Set.elements with
                | [] -> failwith ("impossible " ^ __LOC__)
                | edge :: _ -> OnConflict (edge.source.id, edge.position))
            | Unicycle edge -> OnConflict (edge.source.id, edge.position)
            | Multiparent edge ->
                InConflict (edge.source.id, edge.position, edge.target.id)
            | Conflict _ | Hole (_, _) -> failwith ("impossible " ^ __LOC__))
        | Hole (source, position) -> OnHole (source.id, position)
      in
      (graph, graph_cursor)
  | ZLamP (ingraph, zpat, typ, exp) ->
      let pat_graph, graph_cursor = ZPat.recomp zpat in
      let typ_graph = Typ.recomp typ in
      let exp_graph = Exp.recomp exp in
      let graph = Graph.union4 ingraph.graph pat_graph typ_graph exp_graph in
      (graph, graph_cursor)
  | ZLamT (ingraph, pat, ztyp, exp) ->
      let pat_graph = Pat.recomp pat in
      let typ_graph, graph_cursor = ZTyp.recomp ztyp in
      let exp_graph = Exp.recomp exp in
      let graph = Graph.union4 ingraph.graph pat_graph typ_graph exp_graph in
      (graph, graph_cursor)
  | ZLamE (ingraph, pat, typ, zexp) ->
      let pat_graph = Pat.recomp pat in
      let typ_graph = Typ.recomp typ in
      let exp_graph, graph_cursor = recomp zexp in
      let graph = Graph.union4 ingraph.graph pat_graph typ_graph exp_graph in
      (graph, graph_cursor)
  | ZAppE1 (ingraph, zexp1, exp2)
  | ZPlusE1 (ingraph, zexp1, exp2)
  | ZTimesE1 (ingraph, zexp1, exp2) ->
      let exp1_graph, graph_cursor = recomp zexp1 in
      let exp2_graph = Exp.recomp exp2 in
      let graph = Graph.union3 ingraph.graph exp1_graph exp2_graph in
      (graph, graph_cursor)
  | ZAppE2 (ingraph, exp1, zexp2)
  | ZPlusE2 (ingraph, exp1, zexp2)
  | ZTimesE2 (ingraph, exp1, zexp2) ->
      let exp1_graph = Exp.recomp exp1 in
      let exp2_graph, graph_cursor = recomp zexp2 in
      let graph = Graph.union3 ingraph.graph exp1_graph exp2_graph in
      (graph, graph_cursor)
  | ZConflict (zexp, conflict) ->
      let graph =
        let f exp graph = Graph.union2 (Exp.recomp exp) graph in
        Exp.C.fold f conflict Graph.empty
      in
      let _, graph_cursor = recomp zexp in
      (graph, graph_cursor)

(* let rec follow_cursor : t -> GroveLang.position list = function
  | Cursor _ -> []
  | ZLamP (_, zpat, _, _) -> LamParam :: ZPat.follow_cursor zpat
  | ZLamT (_, _, ztyp, _) -> LamType :: ZTyp.follow_cursor ztyp
  | ZLamE (_, _, _, zexp) -> LamBody :: follow_cursor zexp
  | ZAppE1 (_, zexp, _) -> AppFun :: follow_cursor zexp
  | ZAppE2 (_, _, zexp) -> AppArg :: follow_cursor zexp
  | ZPlusE1 (_, zexp, _) -> PlusLeft :: follow_cursor zexp
  | ZPlusE2 (_, _, zexp) -> PlusRight :: follow_cursor zexp
  | ZTimesE1 (_, zexp, _) -> TimesLeft :: follow_cursor zexp
  | ZTimesE2 (_, _, zexp) -> TimesRight :: follow_cursor zexp
  | ZConflict (zexp, _) -> follow_cursor zexp *)

(* let rec place_cursor (path : GroveLang.position list) (exp : Exp.t) : t option =
  match path with
  | [] -> Some (Cursor exp)
  | position :: path -> (
      match exp with
      | Var (_, _) | Num (_, _) | Multiparent _ | Unicycle _ | Hole (_, _) ->
          None
      | Lam (ingraph, pat1, typ2, exp3) -> (
          match position with
          | LamParam ->
              let+ zpat1 = ZPat.place_cursor path pat1 in
              ZLamP (ingraph, zpat1, typ2, exp3)
          | LamType ->
              let+ ztyp2 = ZTyp.place_cursor path typ2 in
              ZLamT (ingraph, pat1, ztyp2, exp3)
          | LamBody ->
              let+ zexp3 = place_cursor path exp3 in
              ZLamE (ingraph, pat1, typ2, zexp3)
          | _ -> None)
      | App (ingraph, exp1, exp2) -> (
          match position with
          | AppFun ->
              let+ zexp1 = place_cursor path exp1 in
              ZAppE1 (ingraph, zexp1, exp2)
          | AppArg ->
              let+ zexp2 = place_cursor path exp1 in
              ZAppE2 (ingraph, exp1, zexp2)
          | _ -> None)
      | Plus (ingraph, exp1, exp2) -> (
          match position with
          | PlusLeft ->
              let+ zexp1 = place_cursor path exp1 in
              ZPlusE1 (ingraph, zexp1, exp2)
          | PlusRight ->
              let+ zexp2 = place_cursor path exp1 in
              ZPlusE2 (ingraph, exp1, zexp2)
          | _ -> None)
      | Times (ingraph, exp1, exp2) -> (
          match position with
          | TimesLeft ->
              let+ zexp1 = place_cursor path exp1 in
              ZTimesE1 (ingraph, zexp1, exp2)
          | TimesRight ->
              let+ zexp2 = place_cursor path exp1 in
              ZTimesE2 (ingraph, exp1, zexp2)
          | _ -> None)
      | Conflict conflict -> 
        
        ) *)

let rec id : t -> Vertex.id option = function
  | Cursor exp -> Exp.id exp
  | ZLamP (ingraph, _, _, _)
  | ZLamT (ingraph, _, _, _)
  | ZLamE (ingraph, _, _, _)
  | ZAppE1 (ingraph, _, _)
  | ZAppE2 (ingraph, _, _)
  | ZPlusE1 (ingraph, _, _)
  | ZPlusE2 (ingraph, _, _)
  | ZTimesE1 (ingraph, _, _)
  | ZTimesE2 (ingraph, _, _) ->
      Some ingraph.invertex.id
  | ZConflict (zexp1, _) -> id zexp1

(* let move (move_action : UserAction.move) (zexp : t) : t option =
  let rec up : t -> t option = function
    | Cursor _ -> None
    | ZLamP (ingraph, Cursor pat, typ, exp)
    | ZLamT (ingraph, pat, Cursor typ, exp)
    | ZLamE (ingraph, pat, typ, Cursor exp) ->
        Some (Cursor (Lam (ingraph, pat, typ, exp)))
    | ZAppE1 (ingraph, Cursor exp1, exp2) | ZAppE2 (ingraph, exp1, Cursor exp2)
      ->
        Some (Cursor (App (ingraph, exp1, exp2)))
    | ZPlusE1 (ingraph, Cursor exp1, exp2) | ZPlusE2 (ingraph, exp1, Cursor exp2)
      ->
        Some (Cursor (Plus (ingraph, exp1, exp2)))
    | ZTimesE1 (ingraph, Cursor exp1, exp2)
    | ZTimesE2 (ingraph, exp1, Cursor exp2) ->
        Some (Cursor (Times (ingraph, exp1, exp2)))
    | ZLamP (ingraph, zpat, typ, exp) ->
        let+ zpat = ZPat.move move_action zpat in
        ZLamP (ingraph, zpat, typ, exp)
    | ZLamT (ingraph, pat, ztyp, exp) ->
        let+ ztyp = ZTyp.move move_action ztyp in
        ZLamT (ingraph, pat, ztyp, exp)
    | ZLamE (ingraph, pat, typ, zexp) ->
        let+ zexp = up zexp in
        ZLamE (ingraph, pat, typ, zexp)
    | ZAppE1 (ingraph, zexp1, exp2) ->
        let+ zexp1 = up zexp1 in
        ZAppE1 (ingraph, zexp1, exp2)
    | ZAppE2 (ingraph, exp1, zexp2) ->
        let+ zexp2 = up zexp2 in
        ZAppE2 (ingraph, exp1, zexp2)
    | ZPlusE1 (ingraph, zexp1, exp2) ->
        let+ zexp1 = up zexp1 in
        ZPlusE1 (ingraph, zexp1, exp2)
    | ZPlusE2 (ingraph, exp1, zexp2) ->
        let+ zexp2 = up zexp2 in
        ZPlusE2 (ingraph, exp1, zexp2)
    | ZTimesE1 (ingraph, zexp1, exp2) ->
        let+ zexp1 = up zexp1 in
        ZTimesE1 (ingraph, zexp1, exp2)
    | ZTimesE2 (ingraph, exp1, zexp2) ->
        let+ zexp2 = up zexp2 in
        ZTimesE2 (ingraph, exp1, zexp2)
    | ZConflict (Cursor exp, conflict) ->
        Some (Cursor (Conflict (Exp.C.add exp conflict)))
    | ZConflict (zexp1, conflict) ->
        let+ zexp1 = up zexp1 in
        ZConflict (zexp1, conflict)
  in

  let rec down : t -> t option = function
    | Cursor exp -> (
        match exp with
        | Var (_, _) | Num (_, _) | Multiparent _ | Unicycle _ | Hole (_, _) ->
            None
        | Lam (ingraph, pat1, typ2, exp3) ->
            Some (ZLamP (ingraph, Cursor pat1, typ2, exp3))
        | App (ingraph, exp1, exp2) ->
            Some (ZAppE1 (ingraph, Cursor exp1, exp2))
        | Plus (ingraph, exp1, exp2) ->
            Some (ZPlusE1 (ingraph, Cursor exp1, exp2))
        | Times (ingraph, exp1, exp2) ->
            Some (ZTimesE1 (ingraph, Cursor exp1, exp2))
        | Conflict conflict ->
            let+ exp1 = Exp.C.min_elt_opt conflict in
            ZConflict (Cursor exp1, conflict))
    | ZLamP (ingraph, zpat1, typ2, exp3) ->
        let+ zpat1 = ZPat.move move_action zpat1 in
        ZLamP (ingraph, zpat1, typ2, exp3)
    | ZLamT (ingraph, pat1, ztyp2, exp3) ->
        let+ ztyp2 = ZTyp.move move_action ztyp2 in
        ZLamT (ingraph, pat1, ztyp2, exp3)
    | ZLamE (ingraph, pat1, typ2, zexp3) ->
        let+ zexp3 = down zexp3 in
        ZLamE (ingraph, pat1, typ2, zexp3)
    | ZAppE1 (ingraph, zexp1, exp2) ->
        let+ zexp1 = down zexp1 in
        ZAppE1 (ingraph, zexp1, exp2)
    | ZAppE2 (ingraph, exp1, zexp2) ->
        let+ zexp2 = down zexp2 in
        ZAppE2 (ingraph, exp1, zexp2)
    | ZPlusE1 (ingraph, zexp1, exp2) ->
        let+ zexp1 = down zexp1 in
        ZPlusE1 (ingraph, zexp1, exp2)
    | ZPlusE2 (ingraph, exp1, zexp2) ->
        let+ zexp2 = down zexp2 in
        ZPlusE2 (ingraph, exp1, zexp2)
    | ZTimesE1 (ingraph, zexp1, exp2) ->
        let+ zexp1 = down zexp1 in
        ZTimesE1 (ingraph, zexp1, exp2)
    | ZTimesE2 (ingraph, exp1, zexp2) ->
        let+ zexp2 = down zexp2 in
        ZTimesE2 (ingraph, exp1, zexp2)
    | ZConflict (zexp1, conflict) ->
        let+ zexp1 = down zexp1 in
        ZConflict (zexp1, conflict)
  in

  let rec left : t -> t option = function
    | Cursor _
    | ZLamP (_, Cursor _, _, _)
    | ZAppE1 (_, Cursor _, _)
    | ZPlusE1 (_, Cursor _, _)
    | ZTimesE1 (_, Cursor _, _) ->
        None
    | ZLamT (ingraph, pat1, Cursor typ2, exp3) ->
        Some (ZLamP (ingraph, Cursor pat1, typ2, exp3))
    | ZLamE (ingraph, pat1, typ2, Cursor exp3) ->
        Some (ZLamT (ingraph, pat1, Cursor typ2, exp3))
    | ZAppE2 (ingraph, exp1, Cursor exp2) ->
        Some (ZAppE1 (ingraph, Cursor exp1, exp2))
    | ZPlusE2 (ingraph, exp1, Cursor exp2) ->
        Some (ZPlusE1 (ingraph, Cursor exp1, exp2))
    | ZTimesE2 (ingraph, exp1, Cursor exp2) ->
        Some (ZTimesE1 (ingraph, Cursor exp1, exp2))
    | ZLamP (ingraph, zpat1, typ2, exp3) ->
        let+ zpat1 = ZPat.move move_action zpat1 in
        ZLamP (ingraph, zpat1, typ2, exp3)
    | ZLamT (ingraph, pat1, ztyp2, exp3) ->
        let+ ztyp2 = ZTyp.move move_action ztyp2 in
        ZLamT (ingraph, pat1, ztyp2, exp3)
    | ZLamE (ingraph, pat1, typ2, zexp3) ->
        let+ zexp3 = left zexp3 in
        ZLamE (ingraph, pat1, typ2, zexp3)
    | ZAppE1 (ingraph, zexp1, exp2) ->
        let+ zexp1 = left zexp1 in
        ZAppE1 (ingraph, zexp1, exp2)
    | ZAppE2 (ingraph, exp1, zexp2) ->
        let+ zexp2 = left zexp2 in
        ZAppE2 (ingraph, exp1, zexp2)
    | ZPlusE1 (ingraph, zexp1, exp2) ->
        let+ zexp1 = left zexp1 in
        ZPlusE1 (ingraph, zexp1, exp2)
    | ZPlusE2 (ingraph, exp1, zexp2) ->
        let+ zexp2 = left zexp2 in
        ZPlusE2 (ingraph, exp1, zexp2)
    | ZTimesE1 (ingraph, zexp1, exp2) ->
        let+ zexp1 = left zexp1 in
        ZTimesE1 (ingraph, zexp1, exp2)
    | ZTimesE2 (ingraph, exp1, zexp2) ->
        let+ zexp2 = left zexp2 in
        ZTimesE2 (ingraph, exp1, zexp2)
    | ZConflict (zexp1, conflict) ->
        let+ zexp1 = left zexp1 in
        ZConflict (zexp1, conflict)
  in

  let rec right : t -> t option = function
    | Cursor _
    | ZLamE (_, _, _, _)
    | ZAppE2 (_, _, Cursor _)
    | ZPlusE2 (_, _, Cursor _)
    | ZTimesE2 (_, _, Cursor _) ->
        None
    | ZLamP (ingraph, Cursor pat1, typ2, exp3) ->
        Some (ZLamT (ingraph, pat1, Cursor typ2, exp3))
    | ZLamT (ingraph, pat1, Cursor typ2, exp3) ->
        Some (ZLamE (ingraph, pat1, typ2, Cursor exp3))
    | ZAppE1 (ingraph, Cursor exp1, exp2) ->
        Some (ZAppE2 (ingraph, exp1, Cursor exp2))
    | ZPlusE1 (ingraph, Cursor exp1, exp2) ->
        Some (ZPlusE2 (ingraph, exp1, Cursor exp2))
    | ZTimesE1 (ingraph, Cursor exp1, exp2) ->
        Some (ZTimesE2 (ingraph, exp1, Cursor exp2))
    | ZLamP (ingraph, zpat1, typ2, exp3) ->
        let+ zpat1 = ZPat.move move_action zpat1 in
        ZLamP (ingraph, zpat1, typ2, exp3)
    | ZLamT (ingraph, pat1, ztyp2, exp3) ->
        let+ ztyp2 = ZTyp.move move_action ztyp2 in
        ZLamT (ingraph, pat1, ztyp2, exp3)
    | ZAppE1 (ingraph, zexp1, exp2) ->
        let+ zexp1 = right zexp1 in
        ZAppE1 (ingraph, zexp1, exp2)
    | ZAppE2 (ingraph, exp1, zexp2) ->
        let+ zexp2 = right zexp2 in
        ZAppE2 (ingraph, exp1, zexp2)
    | ZPlusE1 (ingraph, zexp1, exp2) ->
        let+ zexp1 = right zexp1 in
        ZPlusE1 (ingraph, zexp1, exp2)
    | ZPlusE2 (ingraph, exp1, zexp2) ->
        let+ zexp2 = right zexp2 in
        ZPlusE2 (ingraph, exp1, zexp2)
    | ZTimesE1 (ingraph, zexp1, exp2) ->
        let+ zexp1 = right zexp1 in
        ZTimesE1 (ingraph, zexp1, exp2)
    | ZTimesE2 (ingraph, exp1, zexp2) ->
        let+ zexp2 = right zexp2 in
        ZTimesE2 (ingraph, exp1, zexp2)
    | ZConflict (zexp1, conflict) ->
        let+ zexp1 = right zexp1 in
        ZConflict (zexp1, conflict)
  in

  (* TODO: implement ZExp.move select *)
  let select () : t option = None in

  match move_action with
  | Up -> up zexp
  | Down -> down zexp
  | Left -> left zexp
  | Right -> right zexp
  | Select _ -> select () *)

(* let rec edit (edit_action : UserAction.edit) (zexp : t) (u_gen : Id.Gen.t) :
    (GraphAction.t list * Id.Gen.t) option =
  match zexp with
  | Cursor exp -> Exp.edit edit_action exp u_gen
  | ZLamP (_, zpat, _, _) -> ZPat.edit edit_action zpat u_gen
  | ZLamT (_, _, zty, _) -> ZTyp.edit edit_action zty u_gen
  | ZLamE (_, _, _, zexp)
  | ZAppE1 (_, zexp, _)
  | ZAppE2 (_, _, zexp)
  | ZPlusE1 (_, zexp, _)
  | ZPlusE2 (_, _, zexp)
  | ZTimesE1 (_, zexp, _)
  | ZTimesE2 (_, _, zexp)
  | ZConflict (zexp, _) ->
      edit edit_action zexp u_gen *)
