open OptionUtil.Syntax

type t = Cursor of Pat.t | ZConflict of t * Pat.C.t [@@deriving sexp]

let rec cursor_position : t -> GroveLang.Position.t option = function
  | Cursor _ -> None
  | ZConflict (Cursor pat, _) ->
      let* ingraph = Pat.ingraph pat in
      Ingraph.inposition ingraph
  | ZConflict (zpat, _) -> cursor_position zpat

let rec erase_cursor : t -> Pat.t = function
  | Cursor p -> p
  | ZConflict (zp, conflict) ->
      Pat.Conflict (Pat.C.add (erase_cursor zp) conflict)

let rec follow_cursor : t -> GroveLang.Position.t list = function
  | Cursor _ -> []
  | ZConflict (zpat, _) -> follow_cursor zpat

let rec id : t -> Vertex.id option = function
  | Cursor pat -> Pat.id pat
  | ZConflict (zpat1, _) -> id zpat1

(* let move (move_action : UserAction.move) (zpat : t) : t option =
  let rec up : t -> t option = function
    | Cursor _ -> None
    | ZConflict (Cursor pat1, conflict) ->
        Some (Cursor (Conflict (Pat.C.add pat1 conflict)))
    | ZConflict (zpat1, conflict) ->
        let+ zpat1 = up zpat1 in
        ZConflict (zpat1, conflict)
  in

  let rec down : t -> t option = function
    | Cursor pat -> (
        match pat with
        | Var (_, _) | Multiparent _ | Unicycle _ | Hole _ -> None
        | Conflict conflict ->
            let+ pat1 = Pat.C.min_elt_opt conflict in
            ZConflict (Cursor pat1, conflict))
    | ZConflict (zpat1, conflict) ->
        let+ zpat1 = down zpat1 in
        ZConflict (zpat1, conflict)
  in

  let rec left : t -> t option = function
    | Cursor _ -> None
    | ZConflict (Cursor pat1, conflict) ->
        let lesser, is_present, _ = Pat.C.split pat1 conflict in
        if not is_present then None
        else
          let* pat1' = Pat.C.max_elt_opt lesser in
          if pat1 = pat1' then None
          else Some (ZConflict (Cursor pat1', conflict))
    | ZConflict (zpat1, conflict) ->
        let+ zpat1 = left zpat1 in
        ZConflict (zpat1, conflict)
  in

  let rec right : t -> t option = function
    | Cursor _ -> None
    | ZConflict (Cursor pat1, conflict) ->
        let _, is_present, greater = Pat.C.split pat1 conflict in
        if not is_present then None
        else
          let* pat1' = Pat.C.min_elt_opt greater in
          if pat1 = pat1' then None
          else Some (ZConflict (Cursor pat1', conflict))
    | ZConflict (zpat1, conflict) ->
        let+ zpat1 = right zpat1 in
        ZConflict (zpat1, conflict)
  in

  (* TODO: implement ZPat.move select *)
  let select () : t option = None in

  match move_action with
  | Up -> up zpat
  | Down -> down zpat
  | Left -> left zpat
  | Right -> right zpat
  | Select _ -> select ()

let rec edit (edit_action : UserAction.edit) (zpat : t) (u_gen : Id.Gen.t) :
    (GraphAction.t list * Id.Gen.t) option =
  match zpat with
  | Cursor pat -> Pat.edit edit_action pat u_gen
  | ZConflict (zpat, _) -> edit edit_action zpat u_gen *)
