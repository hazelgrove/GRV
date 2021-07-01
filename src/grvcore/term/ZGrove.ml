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

let move (move_action : UserAction.move) (zgrove : t) : t option =
  match zgrove with
  | ZGroveTS1 { znoparents; _ } ->
      let* znoparents = ZTerm.Set.move move_action znoparents in
      { zgrove with znoparents }
  | ZGroveTS2 { zmultiparents; _ } ->
      let* zmultiparents = ZTerm.Set.move move_action zmultiparents in
      { zgrove with multiparents }
  | ZGroveTS3 { zunicycles; _ } ->
      let* zunicycles = ZTerm.Set.move move_action zunicycles in
      { zgrove with zunicycles }

let construct (constructor : GroveLang.constructor) (zgrove : t) : t option =
  match zgrove with
  | ZGroveTS1 { znoparents; _ } ->
      let* znoparents = ZTerm.Set.construct constructor znoparents in
      { zgrove with znoparents }
  | ZGroveTS2 { zmultiparents; _ } ->
      let* zmultiparents = ZTerm.Set.construct constructor zmultiparents in
      { zgrove with multiparents }
  | ZGroveTS3 { zunicycles; _ } ->
      let* zunicycles = ZTerm.Set.construct constructor zunicycles in
      { zgrove with zunicycles }

let delete (zgrove : t) : t option =
  match zgrove with
  | ZGroveTS1 { znoparents; _ } ->
      let* znoparents = ZTerm.Set.delete znoparents in
      { zgrove with znoparents }
  | ZGroveTS2 { zmultiparents; _ } ->
      let* zmultiparents = ZTerm.Set.delete zmultiparents in
      { zgrove with multiparents }
  | ZGroveTS3 { zunicycles; _ } ->
      let* zunicycles = ZTerm.Set.delete zunicycles in
      { zgrove with zunicycles }

let relocate (vertex : Vertex.t) (position : GroveLang.position) (zgrove : t) :
    t option =
  match zgrove with
  | ZGroveTS1 { znoparents; _ } ->
      let* znoparents = ZTerm.Set.relocate vertex position znoparents in
      { zgrove with znoparents }
  | ZGroveTS2 { zmultiparents; _ } ->
      let* zmultiparents = ZTerm.Set.relocate vertex position zmultiparents in
      { zgrove with multiparents }
  | ZGroveTS3 { zunicycles; _ } ->
      let* zunicycles = ZTerm.Set.relocate vertex position zunicycles in
      { zgrove with zunicycles }
