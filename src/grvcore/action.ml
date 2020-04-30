(* TODO: factor App and other ast constructor insertions into a constructor *)

type t = Create | Move of direction [@@deriving sexp_of]

and direction = In | Out | Left | Right [@@deriving sexp_of]

let apply (model : Model.t) (action : t) (_state : State.t)
    ~schedule_action:(_ : t -> unit) : Model.t =
  let open Ast in
  let open Ast.HExp in
  let make_app () =
    apply_at model.ast model.cursor (fun exp ->
        Uuid.wrap @@ App (exp, Uuid.wrap EmptyHole))
  in
  let move_in () =
    match Uuid.unwrap (walk_to model.ast model.cursor) with
    | App _ -> Cursor.push Left model.cursor
    | EmptyHole -> model.cursor
  in
  let move_out () = Cursor.pop model.cursor in
  let move_left () =
    match Cursor.last model.cursor with
    | To (Right, _) -> Cursor.unwind model.cursor Left
    | To (Left, _) | Here -> model.cursor
  in
  let move_right () =
    match Cursor.last model.cursor with
    | To (Left, _) -> Cursor.unwind model.cursor Right
    | To (Right, _) | Here -> model.cursor
  in
  match action with
  | Create -> { model with ast = make_app () }
  | Move In -> { model with cursor = move_in () }
  | Move Out -> { model with cursor = move_out () }
  | Move Left -> { model with cursor = move_left () }
  | Move Right -> { model with cursor = move_right () }
