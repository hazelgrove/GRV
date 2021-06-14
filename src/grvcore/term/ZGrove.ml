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
