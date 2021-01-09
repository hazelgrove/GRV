type t = Vertex of Vertex.t * children Position_map.t | Ref of Vertex.t

and children = child list

and child = { edge_id : Uuid.Id.t; tree : t }

let child (edge_id : Uuid.Id.t) (tree : t) : child = { edge_id; tree }

let child_map :
    (Lang.Position.t * (Uuid.Id.t * t) list) list -> children Position_map.t =
  List.fold_left
    (fun children (position, child_specs) ->
      List.fold_left
        (fun children (edge_id, tree) ->
          Position_map.push position (child edge_id tree) children)
        children child_specs)
    Position_map.empty

let vertex (vertex : Vertex.t)
    (children : (Lang.Position.t * (Uuid.Id.t * t) list) list) : t =
  Vertex (vertex, child_map children)

let rec to_string : t -> string = function
  | Vertex (vertex, map) -> (
      let children =
        Position_map.bindings map
        |> List.map (fun (position, tree_children) ->
               Format.sprintf "%s -> %s"
                 (Lang.Position.short_name position)
                 ( match List.length tree_children with
                 | 0 -> "□"
                 | 1 ->
                     let child = List.hd tree_children in
                     to_string child.tree
                 | _ ->
                     let childs =
                       List.map (fun { tree; _ } -> tree) tree_children
                     in
                     "{"
                     ^ (List.map to_string childs |> String.concat " | ")
                     ^ "}" ))
        |> String.concat "; "
      in
      let id = Uuid.Id.to_string vertex.id in
      match children with
      | "" -> Format.sprintf "Vertex(%s)" id
      | _ -> Format.sprintf "Vertex(%s, %s)" id children )
  | Ref vertex -> "#" ^ Uuid.Id.to_string vertex.id
