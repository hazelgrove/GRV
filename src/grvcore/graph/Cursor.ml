type t = { vertex : Vertex.t; position : Lang.Position.t } [@@deriving sexp]

let to_string (cursor : t) : string =
  Format.sprintf "%s.%s"
    (Uuid.Id.to_string cursor.vertex.id)
    (Lang.Position.short_name cursor.position)

let root : t = { vertex = Vertex.root; position = Root_root_root }

(* Tests for Cursors *)
let%test "root cursor has UUID 0" = Uuid.Id.to_string root.vertex.id = "0"

let%test "root cursor has position Root_root_root" =
  Lang.Position.equal root.position Root_root_root

let%test "cursor representation" = to_string root = "0.root"
