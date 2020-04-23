open Ast.HExp
open Cursor

let wrap, unwrap = (Uuid.wrap, Uuid.unwrap)

let apply_action (model : Model.t) (action : Action.t) (_state : State.t)
    ~schedule_action:(_ : Action.t -> unit) : Model.t =
  let program, cursor = (model.ast, model.cursor) in
  let make_app () =
    apply_at program cursor (fun exp -> wrap @@ App (exp, wrap EmptyHole))
  in
  let move_in () =
    match unwrap (walk_to program cursor) with
    | EmptyHole -> cursor
    | App _ -> push Left cursor
  in
  let move_out () = pop cursor in
  let move_left () =
    match last_side_of cursor with
    | None | Some Left -> cursor
    | Some Right -> look_to Left cursor
  in
  let move_right () =
    match last_side_of cursor with
    | None | Some Right -> cursor
    | Some Left -> look_to Right cursor
  in
  match action with
  | App -> { model with ast = make_app () }
  | Move In -> { model with cursor = move_in () }
  | Move Out -> { model with cursor = move_out () }
  | Move Left -> { model with cursor = move_left () }
  | Move Right -> { model with cursor = move_right () }
