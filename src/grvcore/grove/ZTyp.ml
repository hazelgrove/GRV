open OptionUtil.Syntax

type t =
  | Cursor of Typ.t
  | ZArrowT1 of Ingraph.t * t * Typ.t
  | ZArrowT2 of Ingraph.t * Typ.t * t
  | ZConflict of t * Typ.C.t
[@@deriving sexp]

let rec cursor_position : t -> GroveLang.Position.t option = function
  | Cursor _ -> None
  | ZArrowT1 (_, Cursor _, _) -> Some ArrowArg
  | ZArrowT2 (_, _, Cursor _) -> Some ArrowResult
  | ZConflict (Cursor typ, _) ->
      let* ingraph = Typ.ingraph typ in
      Ingraph.inposition ingraph
  | ZArrowT1 (_, ztyp, _) | ZArrowT2 (_, _, ztyp) | ZConflict (ztyp, _) ->
      cursor_position ztyp

let rec cursor_sort : t -> GroveLang.Sort.t = function
  | Cursor _ -> Typ
  | ZArrowT1 (_, ztyp, _) | ZArrowT2 (_, _, ztyp) | ZConflict (ztyp, _) ->
      cursor_sort ztyp

let rec erase_cursor : t -> Typ.t = function
  | Cursor ty -> ty
  | ZArrowT1 (ingraph, zty1, ty2) -> Arrow (ingraph, erase_cursor zty1, ty2)
  | ZArrowT2 (ingraph, ty1, zty2) -> Arrow (ingraph, ty1, erase_cursor zty2)
  | ZConflict (zty, conflict) ->
      Typ.Conflict (Typ.C.add (erase_cursor zty) conflict)

let rec follow_cursor : t -> GroveLang.Position.t list = function
  | Cursor _ -> []
  | ZArrowT1 (_, ztyp, _) -> ArrowArg :: follow_cursor ztyp
  | ZArrowT2 (_, _, ztyp) -> ArrowResult :: follow_cursor ztyp
  | ZConflict (zpat, _) -> follow_cursor zpat

let rec id : t -> Vertex.id option = function
  | Cursor typ -> Typ.id typ
  | ZArrowT1 (ingraph, _, _) | ZArrowT2 (ingraph, _, _) ->
      Some ingraph.invertex.id
  | ZConflict (ztyp1, _) -> id ztyp1

let rec recomp : t -> Graph.t * GraphCursor.t = function
  | Cursor typ ->
      let graph = Typ.recomp typ in
      let graph_cursor =
        let open GraphCursor in
        match typ with
        | Num ingraph | Arrow (ingraph, _, _) -> OnVertex ingraph.invertex.id
        | Multiparent edge | Unicycle edge ->
            OnRef (edge.source.id, edge.position)
        | Conflict conflict -> (
            match Typ.C.choose conflict with
            | Num ingraph | Arrow (ingraph, _, _) -> (
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
  | ZArrowT1 (ingraph, ztyp1, typ2) ->
      let typ1_graph, graph_cursor = recomp ztyp1 in
      let typ2_graph = Typ.recomp typ2 in
      let graph = Graph.union3 ingraph.graph typ1_graph typ2_graph in
      (graph, graph_cursor)
  | ZArrowT2 (ingraph, typ1, ztyp2) ->
      let typ1_graph = Typ.recomp typ1 in
      let typ2_graph, graph_cursor = recomp ztyp2 in
      let graph = Graph.union3 ingraph.graph typ1_graph typ2_graph in
      (graph, graph_cursor)
  | ZConflict (ztyp, conflict) ->
      let graph =
        let f typ graph = Graph.union2 (Typ.recomp typ) graph in
        Typ.C.fold f conflict Graph.empty
      in
      let _, graph_cursor = recomp ztyp in
      (graph, graph_cursor)

(* let rec edit (edit_action : UserAction.edit) (ztyp : t) (u_gen : Id.Gen.t) :
    (GraphAction.t list * Id.Gen.t) option =
  match ztyp with
  | Cursor typ -> Typ.edit edit_action typ u_gen
  | ZArrowT1 (_, ztyp, _) | ZArrowT2 (_, _, ztyp) | ZConflict (ztyp, _) ->
      edit edit_action ztyp u_gen

let move (move_action : UserAction.move) (ztyp : t) : t option =
  let rec up : t -> t option = function
    | Cursor _ -> None
    | ZArrowT1 (ingraph, Cursor typ1, typ2)
    | ZArrowT2 (ingraph, typ1, Cursor typ2) ->
        Some (Cursor (Arrow (ingraph, typ1, typ2)))
    | ZConflict (Cursor typ1, conflict) ->
        Some (Cursor (Conflict (Typ.C.add typ1 conflict)))
    | ZArrowT1 (ingraph, ztyp1, typ2) ->
        let+ ztyp1 = up ztyp1 in
        ZArrowT1 (ingraph, ztyp1, typ2)
    | ZArrowT2 (ingraph, typ1, ztyp2) ->
        let+ ztyp2 = up ztyp2 in
        ZArrowT2 (ingraph, typ1, ztyp2)
    | ZConflict (ztyp1, conflict) ->
        let+ ztyp1 = up ztyp1 in
        ZConflict (ztyp1, conflict)
  in

  let rec down : t -> t option = function
    | Cursor typ -> (
        match typ with
        | Num _ | Multiparent _ | Unicycle _ | Hole _ -> None
        | Arrow (ingraph, typ1, typ2) ->
            Some (ZArrowT1 (ingraph, Cursor typ1, typ2))
        | Conflict conflict ->
            let+ typ1 = Typ.C.min_elt_opt conflict in
            ZConflict (Cursor typ1, conflict))
    | ZArrowT1 (ingraph, ztyp1, typ2) ->
        let+ ztyp1 = down ztyp1 in
        ZArrowT1 (ingraph, ztyp1, typ2)
    | ZArrowT2 (ingraph, typ1, ztyp2) ->
        let+ ztyp2 = down ztyp2 in
        ZArrowT2 (ingraph, typ1, ztyp2)
    | ZConflict (ztyp1, conflict) ->
        let+ ztyp1 = down ztyp1 in
        ZConflict (ztyp1, conflict)
  in

  let rec left : t -> t option = function
    | Cursor _ | ZArrowT1 (_, Cursor _, _) -> None
    | ZArrowT2 (ingraph, typ1, Cursor typ2) ->
        Some (ZArrowT1 (ingraph, Cursor typ1, typ2))
    | ZConflict (Cursor typ1, conflict) ->
        let lesser, is_present, _ = Typ.C.split typ1 conflict in
        if not is_present then None
        else
          let* typ1' = Typ.C.max_elt_opt lesser in
          if typ1 = typ1' then None
          else Some (ZConflict (Cursor typ1', conflict))
    | ZArrowT1 (ingraph, ztyp1, typ2) ->
        let+ ztyp1 = left ztyp1 in
        ZArrowT1 (ingraph, ztyp1, typ2)
    | ZArrowT2 (ingraph, typ1, ztyp2) ->
        let+ ztyp2 = left ztyp2 in
        ZArrowT2 (ingraph, typ1, ztyp2)
    | ZConflict (ztyp1, conflict) ->
        let+ ztyp1 = left ztyp1 in
        ZConflict (ztyp1, conflict)
  in

  let rec right : t -> t option = function
    | Cursor _ | ZArrowT2 (_, _, Cursor _) -> None
    | ZArrowT1 (ingraph, Cursor typ1, typ2) ->
        Some (ZArrowT2 (ingraph, typ1, Cursor typ2))
    | ZConflict (Cursor typ1, conflict) ->
        let _, is_present, greater = Typ.C.split typ1 conflict in
        if not is_present then None
        else
          let* typ1' = Typ.C.min_elt_opt greater in
          if typ1 = typ1' then None
          else Some (ZConflict (Cursor typ1', conflict))
    | ZArrowT1 (ingraph, ztyp1, typ2) ->
        let+ ztyp1 = right ztyp1 in
        ZArrowT1 (ingraph, ztyp1, typ2)
    | ZArrowT2 (ingraph, typ1, ztyp2) ->
        let+ ztyp2 = right ztyp2 in
        ZArrowT2 (ingraph, typ1, ztyp2)
    | ZConflict (ztyp1, conflict) ->
        let+ ztyp1 = right ztyp1 in
        ZConflict (ztyp1, conflict)
  in

  (* TODO: implement ZTyp.move select *)
  let select () : t option = None in

  match move_action with
  | Up -> up ztyp
  | Down -> down ztyp
  | Left -> left ztyp
  | Right -> right ztyp
  | Select _ -> select () *)
