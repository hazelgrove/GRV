open OptionUtil.Syntax

type t =
  | ZGroveTS1 of {
      znoparents : ZTerm.Set.t;
      multiparents : Term.Set.t;
      unicycles : Term.Set.t;
    }
  | ZGroveTS2 of {
      noparents : Term.Set.t;
      zmultiparents : ZTerm.Set.t;
      unicycles : Term.Set.t;
    }
  | ZGroveTS3 of {
      noparents : Term.Set.t;
      multiparents : Term.Set.t;
      zunicycles : ZTerm.Set.t;
    }
[@@deriving sexp]

let empty : t =
  let znoparents =
    ZTerm.Set.singleton (ZExp (Cursor (Hole (Vertex.root, RootRoot))))
  in
  let multiparents = Term.Set.empty in
  let unicycles = Term.Set.empty in
  ZGroveTS1 { znoparents; multiparents; unicycles }

let cursor_position : t -> GroveLang.Position.t option = function
  | ZGroveTS1 { znoparents; _ } -> ZTerm.Set.cursor_position znoparents
  | ZGroveTS2 { zmultiparents; _ } -> ZTerm.Set.cursor_position zmultiparents
  | ZGroveTS3 { zunicycles; _ } -> ZTerm.Set.cursor_position zunicycles

let erase_cursor : t -> Grove.t = function
  | ZGroveTS1 { znoparents; multiparents; unicycles } ->
      let noparents = ZTerm.Set.erase_cursor znoparents in
      { noparents; multiparents; unicycles }
  | ZGroveTS2 { noparents; zmultiparents; unicycles } ->
      let multiparents = ZTerm.Set.erase_cursor zmultiparents in
      { noparents; multiparents; unicycles }
  | ZGroveTS3 { noparents; multiparents; zunicycles } ->
      let unicycles = ZTerm.Set.erase_cursor zunicycles in
      { noparents; multiparents; unicycles }

let find_vertex (vertex_id : Vertex.id) (zgrove : t) : Vertex.t option =
  zgrove |> erase_cursor |> Grove.recomp |> Graph.find_vertex vertex_id

let find_root_opt : t -> ZTerm.Root.t option = function
  | ZGroveTS1 { znoparents; _ } -> ZTerm.Set.find_root znoparents
  | ZGroveTS2 { noparents; _ } | ZGroveTS3 { noparents; _ } ->
      let+ root = Term.Set.find_root noparents in
      ZTerm.Root.Root root

(* let follow_cursor : t -> Vertex.id option * GroveLang.position list = function
  | ZGroveTS1 { znoparents; _ } -> ZTerm.Set.follow_cursor znoparents
  | ZGroveTS2 { zmultiparents; _ } -> ZTerm.Set.follow_cursor zmultiparents
  | ZGroveTS3 { zunicycles; _ } -> ZTerm.Set.follow_cursor zunicycles *)

(* let place_cursor (path : Vertex.id option * GroveLang.position list)
    ({ noparents; multiparents; unicycles } : Grove.t) : t option =
  match ZTerm.Set.place_cursor path noparents with
  | Some znoparents -> Some (ZGroveTS1 { znoparents; multiparents; unicycles })
  | None -> (
      match ZTerm.Set.place_cursor path multiparents with
      | Some zmultiparents ->
          Some (ZGroveTS2 { noparents; zmultiparents; unicycles })
      | None ->
          let+ zunicycles = ZTerm.Set.place_cursor path unicycles in
          ZGroveTS3 { noparents; multiparents; zunicycles }) *)

let recomp (zgrove : t) : Graph.t = Grove.recomp (erase_cursor zgrove)

(* let move (move_action : UserAction.move) (zgrove : t) : t option =
  match zgrove with
  | ZGroveTS1 ({ znoparents; _ } as zgrove') ->
      let+ znoparents = ZTerm.Set.move move_action znoparents in
      ZGroveTS1 { zgrove' with znoparents }
  | ZGroveTS2 ({ zmultiparents; _ } as zgrove') ->
      let+ zmultiparents = ZTerm.Set.move move_action zmultiparents in
      ZGroveTS2 { zgrove' with zmultiparents }
  | ZGroveTS3 ({ zunicycles; _ } as zgrove') ->
      let+ zunicycles = ZTerm.Set.move move_action zunicycles in
      ZGroveTS3 { zgrove' with zunicycles } *)

(* let edit (edit_action : UserAction.edit) (zgrove : t) (u_gen : Id.Gen.t) :
    (GraphAction.t list * Id.Gen.t) option =
  match zgrove with
  | ZGroveTS1 { znoparents; _ } -> ZTerm.Set.edit edit_action znoparents u_gen
  | ZGroveTS2 { zmultiparents; _ } ->
      ZTerm.Set.edit edit_action zmultiparents u_gen
  | ZGroveTS3 { zunicycles; _ } -> ZTerm.Set.edit edit_action zunicycles u_gen *)
