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

let reduce (f : ZTerm.Set.t -> 'a) (zgrove : t) : 'a =
  match (zgrove : t) with
  | ZGroveTS1 { znoparents; _ } -> f znoparents
  | ZGroveTS2 { zmultiparents; _ } -> f zmultiparents
  | ZGroveTS3 { zunicycles; _ } -> f zunicycles

let map (f : ZTerm.Set.t -> Term.Set.t) (zgrove : t) : Grove.t =
  match zgrove with
  | ZGroveTS1 { znoparents; multiparents; unicycles } ->
      let noparents = f znoparents in
      { noparents; multiparents; unicycles }
  | ZGroveTS2 { noparents; zmultiparents; unicycles } ->
      let multiparents = f zmultiparents in
      { noparents; multiparents; unicycles }
  | ZGroveTS3 { noparents; multiparents; zunicycles } ->
      let unicycles = f zunicycles in
      { noparents; multiparents; unicycles }

let cursor_position : t -> GroveLang.Position.t option =
  reduce ZTerm.Set.cursor_position

let cursor_sort : t -> GroveLang.Sort.t = reduce ZTerm.Set.cursor_sort

let cursor_term : t -> Term.t = reduce ZTerm.Set.cursor_term

let erase_cursor : t -> Grove.t = map ZTerm.Set.erase_cursor

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

let recomp (zgrove : t) : Graph.t * GraphCursor.t =
  (* Grove.recomp (erase_cursor zgrove) *)
  match zgrove with
  | ZGroveTS1 { znoparents; multiparents; unicycles } ->
      let noparents_graph, graph_cursor = ZTerm.Set.recomp znoparents in
      let multiparents_graph = Term.Set.recomp multiparents in
      let unicycles_graph = Term.Set.recomp unicycles in
      let graph =
        Graph.union3 noparents_graph multiparents_graph unicycles_graph
      in
      (graph, graph_cursor)
  | ZGroveTS2 { noparents; zmultiparents; unicycles } ->
      let noparents_graph = Term.Set.recomp noparents in
      let multiparents_graph, graph_cursor = ZTerm.Set.recomp zmultiparents in
      let unicycles_graph = Term.Set.recomp unicycles in
      let graph =
        Graph.union3 noparents_graph multiparents_graph unicycles_graph
      in
      (graph, graph_cursor)
  | ZGroveTS3 { noparents; multiparents; zunicycles } ->
      let noparents_graph = Term.Set.recomp noparents in
      let multiparents_graph = Term.Set.recomp multiparents in
      let unicycles_graph, graph_cursor = ZTerm.Set.recomp zunicycles in
      let graph =
        Graph.union3 noparents_graph multiparents_graph unicycles_graph
      in
      (graph, graph_cursor)

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
