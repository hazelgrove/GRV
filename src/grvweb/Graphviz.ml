let red_color = {|"#FF5C56"|}

let orange_color = {|"#FFB643"|}

let green_color = {|"#A4FF77"|}

let blue_color = {|"#87E2FF"|}

let purple_color = {|"#D7BBD9"|}

let vertex_color (vertex : Vertex.t) (graph : Graph.t) (cursor : Cursor.t) :
    string =
  if vertex = Vertex.root then "black"
  else
    let parents = Graph.parent_edges graph vertex in
    if Edge.Set.is_empty parents then "white"
    else if
      Edge.Set.exists
        (fun edge -> Edge.source edge = cursor && Edge.target edge = vertex)
        parents
    then purple_color
    else "white"

let draw_graph (graph : Graph.t) (cursor : Cursor.t) : string =
  let nodes =
    List.map
      (fun (vertex : Vertex.t) ->
        let id = Uuid.Id.show vertex.id in
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
        let fillcolor = vertex_color vertex graph cursor in
        let color =
          let num_parents =
            Edge.Set.cardinal (Graph.parent_edges graph vertex)
          in
          if num_parents < 2 then "black" else orange_color
        in
        Printf.sprintf
          {|n%s [label="{%s: %s|{%s}}",style=filled,fillcolor=%s,color=%s]|} id
          id label children fillcolor color)
      (Vertex.Set.elements (Graph.vertexes graph))
  in
  let edges : string list =
    let live_edges = Graph.live_edges graph in
    Edge.Set.fold
      (fun edge strs ->
        let source_id = Uuid.Id.show edge.value.source.vertex.id in
        let target_id = Uuid.Id.show edge.value.target.id in
        let edge_id = Uuid.Id.show edge.id in
        let index =
          Format.asprintf "%a" Lang.Index.pp edge.value.source.index
        in
        let color =
          let num_conflicts =
            Edge.Set.(
              cardinal
                (filter
                   (fun (e : Edge.t) -> e.value.source = edge.value.source)
                   (remove edge live_edges)))
          in
          let num_parents =
            Edge.Set.cardinal (Graph.parent_edges graph edge.value.target)
          in
          if num_conflicts = 0 && num_parents = 1 then "black"
          else if num_conflicts > 0 then red_color
          else orange_color
        in
        let field = Lang.Index.short_name edge.value.source.index in
        Printf.sprintf
          {|n%s:%s -> n%s [color=%s,label="%s",edgeURL="#",edgetooltip="id: %s\nsource: %s\nindex: %s\ntarget: %s",labeltooltip="id: %s\nsource: %s\nindex: %s\ntarget: %s"]|}
          source_id field target_id color edge_id edge_id source_id index
          target_id edge_id source_id index target_id
        :: strs)
      live_edges []
  in
  let hole, hole_edge =
    let children = Graph.cursor_children graph cursor in
    match Edge.Set.is_empty children with
    | true ->
        let field = Lang.Index.short_name cursor.index in
        let open Printf in
        ( [
            sprintf {|hole [label="",shape=circle,style=filled,fillcolor=%s]|}
              purple_color;
          ],
          [ sprintf "n%s:%s -> hole" (Uuid.Id.show cursor.vertex.id) field ] )
    | false -> ([], [])
  in
  {|digraph G {
   node [shape=Mrecord,fontsize=11,ranksep=0];
   edge [arrowhead=vee,fontsize=11,weight=2];
   rankdir = LR;
   {rank=min; n0 [shape=point]};
   |}
  ^ String.concat ";\n" (nodes @ hole @ edges @ hole_edge)
  ^ "}"

let draw (editor : Editor.t) : unit =
  Js.draw_viz editor.id (String.escaped @@ draw_graph editor.graph editor.cursor)
