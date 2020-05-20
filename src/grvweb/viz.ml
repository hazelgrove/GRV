module Js = Js_of_ocaml.Js

let field_of_index : Lang.Index.t -> string Option.t = function
  | Root_root_root -> None
  | Exp_lam_param -> Some "param"
  | Exp_lam_param_type -> Some "param_type"
  | Exp_lam_body -> Some "body"
  | Exp_app_fun -> Some "fun"
  | Exp_app_arg -> Some "arg"
  | Exp_plus_left -> Some "left"
  | Exp_plus_right -> Some "right"
  | Typ_arrow_arg -> Some "arg"
  | Typ_arrow_result -> Some "result"

let vertex_label (vertex : Vertex.t) (id : string) : string =
  match Uuid.Wrap.unmk vertex with
  | Root_root -> "Root_root"
  | Pat_var str -> "Pat_var " ^ id ^ "\n" ^ str
  | Exp_var str -> "Exp_var " ^ id ^ "\n" ^ str
  | Exp_lam ->
      "{Exp_lam " ^ id ^ "|"
      ^ {|{<param> param
         | <param_type> param_type
         | <body> body}}
         |}
  | Exp_num num -> "Exp_num " ^ id ^ "\n" ^ string_of_int num
  | Exp_app -> "{Exp_app " ^ id ^ "|{<fun> fun|<arg> arg}}"
  | Exp_plus -> "{Exp_plus " ^ id ^ "|{<left> left|<right> right}}"
  | Typ_arrow -> "{Typ_arrow " ^ id ^ "|{<arg> arg|<result> result}}"
  | Typ_num -> "Typ_num " ^ id

let cursor_color = {|"#ddffdd"|}

let vertex_color (vertex : Vertex.t) (cursor : Cursor.t) (cache : Cache.t) :
    string =
  if vertex = Vertex.root then "black"
  else
    match Vertex.Map.find_opt vertex cache.parents with
    | None -> "white"
    | Some edges ->
        if
          List.exists
            (fun edge -> Edge.source edge = cursor && Edge.target edge = vertex)
            (List.of_seq @@ Edge.Set.to_seq edges)
        then cursor_color
        else "white"

let draw_graph (graph : Graph.t) (cursor : Cursor.t) : string =
  let nodes =
    let has_source_vertex (vertex : Vertex.t) (edge : Edge.t) : bool =
      (Edge.source edge).vertex = vertex
    in
    let is_live_vertex _ vertex =
      (not (Edge.Set.is_empty @@ Cache.parents vertex graph.cache))
      || List.exists
           (Edge.Set.exists @@ has_source_vertex vertex)
           (List.map snd @@ Cursor.Map.bindings graph.cache.children)
    in
    let live_vertexes = Uuid.Map.filter is_live_vertex graph.cache.vertexes in
    List.map (fun (key, vertex) ->
        let id = Uuid.Id.show key in
        let label = vertex_label vertex id in
        let color = vertex_color vertex cursor graph.cache in
        Printf.sprintf {|n%s [label="%s",style=filled,fillcolor=%s]|} id label
          color)
    @@ Uuid.Map.bindings live_vertexes
  in
  let edges =
    let open List in
    let children = map snd (of_seq @@ Cursor.Map.to_seq graph.cache.children) in
    let live_children =
      let is_live edge = Edge.Map.find_opt edge graph.states = Some Created in
      filter (Edge.Set.exists is_live) children
    in
    let live_edges =
      concat @@ map of_seq @@ map Edge.Set.to_seq live_children
    in
    map
      (fun edge ->
        let source : Vertex.t = (Edge.source edge).vertex in
        let target : Vertex.t = Edge.target edge in
        let source_id = Uuid.Id.show source.id in
        let target_id = Uuid.Id.show target.id in
        let color =
          if
            length @@ filter (fun e -> Edge.(source e = source edge)) live_edges
            < 2
          then "black"
          else "red"
        in
        match field_of_index (Edge.source edge).index with
        | None -> Printf.sprintf "n0 -> n%s [color=%s]" target_id color
        | Some field ->
            Printf.sprintf "n%s:%s -> n%s [color=%s]" source_id field target_id
              color)
      live_edges
  in
  let hole, hole_edge =
    let children = Cache.children cursor graph.cache in
    match (children = Edge.Set.empty, field_of_index cursor.index) with
    | true, Some field ->
        let open Printf in
        ( [
            sprintf {|hole [label="",shape=circle,style=filled,fillcolor=%s]|}
              cursor_color;
          ],
          [ sprintf "n%s:%s -> hole" (Uuid.Id.show cursor.vertex.id) field ] )
    | false, _ | _, None -> ([], [])
  in
  {|digraph G {
   node [shape=Mrecord];
   edge [arrowhead=vee];
   {rank=min; n0 [shape=point]};
   |}
  ^ String.concat ";\n" (nodes @ hole @ edges @ hole_edge)
  ^ "}"

let draw (instance : int) (graph : Graph.t) (cursor : Cursor.t) : unit =
  Js.Unsafe.js_expr
  @@ Printf.sprintf "drawViz('graph%d', '%s')" instance
       (String.escaped @@ draw_graph graph cursor)
