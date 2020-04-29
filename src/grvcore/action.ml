(* TODO: factor App and other ast constructor insertions into a constructor *)

module HExp = Ast.HExp

type t = App | Move of direction [@@deriving sexp_of]

and direction = In | Out | Left | Right [@@deriving sexp_of]

let apply (model : Model.t) (action : t) (_state : State.t)
    ~schedule_action:(_ : t -> unit) : Model.t =
  let wrap, unwrap = Ast.(wrap, unwrap) in
  let make_app () =
    HExp.apply_at model.ast model.cursor (fun exp ->
        wrap @@ HExp.App (exp, wrap HExp.EmptyHole))
  in
  let move_in () =
    match unwrap (HExp.walk_to model.ast model.cursor) with
    | App _ -> Cursor.push `Left model.cursor
    | EmptyHole -> model.cursor
  in
  let move_out () = Cursor.pop model.cursor in
  let move_left () =
    match Cursor.last_side_of model.cursor with
    | Some `Right -> Cursor.look_to `Left model.cursor
    | Some `Left | None -> model.cursor
  in
  let move_right () =
    match Cursor.last_side_of model.cursor with
    | Some `Left -> Cursor.look_to `Right model.cursor
    | Some `Right | None -> model.cursor
  in
  match action with
  | App -> { model with ast = make_app () }
  | Move In -> { model with cursor = move_in () }
  | Move Out -> { model with cursor = move_out () }
  | Move Left -> { model with cursor = move_left () }
  | Move Right -> { model with cursor = move_right () }
