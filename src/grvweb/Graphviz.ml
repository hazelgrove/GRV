let cursor_color = {|"#ddffdd"|}

let vertex_color (vertex : Vertex.t) (cursor : Cursor.t) (cache : Cache.t) :
    string =
  if vertex = Vertex.root then "black"
  else
    match Vertex.Map.find_opt vertex cache.parents with
    | None -> "white"
    | Some edges ->
        if
          Edge.Set.exists
            (fun edge -> Edge.source edge = cursor && Edge.target edge = vertex)
            edges
        then cursor_color
        else "white"

let draw_graph (graph : Graph.t) (cursor : Cursor.t) : string =
  let nodes =
    List.map
      (fun (key, vertex) ->
        let id = Uuid.Id.show key in
        let constructor = Uuid.Wrap.unmk vertex in
        let label = Lang.Constructor.graphviz_label constructor in
        let children =
          String.concat "|"
            (List.map
               (fun i ->
                 let name = Lang.Index.short_name i in
                 Printf.sprintf "<%s> %s" name name)
               (Lang.Index.child_indexes constructor))
        in
        let color = vertex_color vertex cursor graph.cache in
        Printf.sprintf {|n%s [label="{%s: %s|{%s}}",style=filled,fillcolor=%s]|}
          id id label children color)
      (Uuid.Map.bindings graph.cache.vertexes)
  in
  let edges =
    let open List in
    let children = map snd (Cursor.Map.bindings graph.cache.children) in
    let live_children =
      let is_live edge = Edge.Map.find_opt edge graph.states = Some Created in
      filter (Edge.Set.exists is_live) children
    in
    let live_edges = concat (map Edge.Set.elements live_children) in
    map
      (fun (edge : Edge.t) ->
        let source : Vertex.t = (Edge.source edge).vertex in
        let target : Vertex.t = Edge.target edge in
        let source_id = Uuid.Id.show source.id in
        let target_id = Uuid.Id.show target.id in
        let color =
          let siblings =
            filter
              (fun (e : Edge.t) -> Edge.source e = Edge.source edge)
              live_edges
          in
          if length siblings < 2 then "black" else "red"
        in
        let field = Lang.Index.short_name (Edge.source edge).index in
        Printf.sprintf "n%s:%s -> n%s [color=%s]" source_id field target_id
          color)
      live_edges
  in
  let hole, hole_edge =
    let children = Cache.children cursor graph.cache in
    match Edge.Set.is_empty children with
    | true ->
        let field = Lang.Index.short_name cursor.index in
        let open Printf in
        ( [
            sprintf {|hole [label="",shape=circle,style=filled,fillcolor=%s]|}
              cursor_color;
          ],
          [ sprintf "n%s:%s -> hole" (Uuid.Id.show cursor.vertex.id) field ] )
    | false -> ([], [])
  in
  {|digraph G {
   node [shape=Mrecord];
   edge [arrowhead=vee];
   {rank=min; n0 [shape=point]};
   |}
  ^ String.concat ";\n" (nodes @ hole @ edges @ hole_edge)
  ^ "}"

let draw (editor : Editor.t) : unit =
  Js.draw_viz editor.id
    (String.escaped @@ draw_graph editor.value.graph editor.value.cursor)
