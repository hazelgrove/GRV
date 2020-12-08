let vertex_color (graph : Graph.t) (cursor : Cursor.t) (vertex : Vertex.t) :
    string =
  let parents = Graph.parent_edges graph vertex in
  let source_is_cursor (edge : Edge.t) =
    edge.value.source = cursor && edge.value.target = vertex
  in
  if vertex = Vertex.root then Color.black
  else if Edge.Set.is_empty parents then Color.white
  else if Edge.Set.exists source_is_cursor parents then Color.purple
  else Color.white

let draw_index (index : Lang.Index.t) : string =
  let name = Lang.Index.short_name index in
  Format.sprintf "<%s> %s" name name

let draw_vertex_children (vertex : Vertex.t) : string =
  Lang.Index.child_indexes vertex.value
  |> List.map draw_index |> String.concat "|"

let draw_vertex (graph : Graph.t) (cursor : Cursor.t) (vertex : Vertex.t) :
    string =
  let id = Uuid.Id.to_string vertex.id in
  Format.sprintf
    {|n%s [label="{%s: %s|{%s}}", style=filled, fillcolor=%s, color=%s]|} id id
    (Lang.Constructor.graphviz_label vertex.value)
    (draw_vertex_children vertex)
    (vertex_color graph cursor vertex)
    Color.(
      if Graph.parent_edges graph vertex |> Edge.Set.cardinal < 2 then black
      else orange)

let draw_edge (graph : Graph.t) (live : Edge.Set.t) (edge : Edge.t) : string =
  let source_id = Uuid.Id.to_string edge.value.source.vertex.id in
  let target_id = Uuid.Id.to_string edge.value.target.id in
  let edge_id = Uuid.Id.to_string edge.id in
  let index = Lang.Index.show edge.value.source.index in
  let field = Lang.Index.short_name edge.value.source.index in
  let color =
    let num_conflicts =
      Edge.Set.(
        remove edge live
        |> filter (fun (e : Edge.t) -> e.value.source = edge.value.source)
        |> cardinal)
    in
    let num_parents =
      Edge.Set.cardinal (Graph.parent_edges graph edge.value.target)
    in
    if num_conflicts = 0 && num_parents = 1 then Color.black
    else if num_conflicts > 0 then Color.red
    else Color.orange
  in
  Format.sprintf
    {|n%s:%s -> n%s [color=%s,label="%s",edgeURL="#",edgetooltip="id: %s\nsource: %s\nindex: %s\ntarget: %s",labeltooltip="id: %s\nsource: %s\nindex: %s\ntarget: %s"]|}
    source_id field target_id color edge_id edge_id source_id index target_id
    edge_id source_id index target_id

let maybe_draw_cursor_hole (graph : Graph.t) (cursor : Cursor.t) :
    string list * string list =
  if Edge.Set.is_empty (Graph.cursor_children graph cursor) then
    ( [
        Format.sprintf
          {|hole [label="",shape=circle,style=filled,fillcolor=%s]|}
          Color.purple;
      ],
      [
        Format.sprintf "n%s:%s -> hole"
          (Uuid.Id.to_string cursor.vertex.id)
          (Lang.Index.short_name cursor.index);
      ] )
  else ([], [])

let draw_graph (graph : Graph.t) (cursor : Cursor.t) : string =
  let nodes =
    Graph.vertexes graph |> Vertex.Set.elements
    |> List.map (draw_vertex graph cursor)
  in
  let edges =
    let live = Graph.live_edges graph in
    live |> Edge.Set.elements |> List.map (draw_edge graph live)
  in
  let hole_node, hole_edge = maybe_draw_cursor_hole graph cursor in
  {|digraph G {
   node [shape=Mrecord,fontsize=11,ranksep=0];
   edge [arrowhead=vee,fontsize=11,weight=2];
   rankdir = LR;
   {rank=min; n0 [shape=point]};
   |}
  ^ String.concat ";\n" (nodes @ hole_node @ edges @ hole_edge)
  ^ "}"

let draw (editor : Editor.t) : unit =
  draw_graph editor.graph editor.cursor
  |> String.escaped |> Js.draw_viz editor.id
