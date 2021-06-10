let vertex_color (graph : Old_Graph.t) (cursor : Cursor.t)
    (vertex : Old_Vertex.t) : string =
  let parents = Old_Graph.parent_edges graph vertex in
  let source_is_cursor (edge : Old_Edge.t) =
    edge.value.source = cursor && edge.value.target = vertex
  in
  if vertex = Old_Vertex.root then Color.black
  else if Old_Edge.Set.is_empty parents then Color.white
  else if Old_Edge.Set.exists source_is_cursor parents then Color.purple
  else Color.white

let draw_position (position : Lang.Position.t) : string =
  let name = Lang.Position.short_name position in
  Format.sprintf "<%s> %s" name name

let draw_vertex_children (vertex : Old_Vertex.t) : string =
  Lang.Position.child_positions vertex.value
  |> List.map draw_position |> String.concat "|"

let draw_vertex (graph : Old_Graph.t) (cursor : Cursor.t)
    (vertex : Old_Vertex.t) : string =
  let id = Uuid.Id.to_string vertex.id in
  Format.sprintf
    {|n%s [label="{%s: %s|{%s}}", style=filled, fillcolor=%s, color=%s]|} id id
    (Lang.Constructor.graphviz_label vertex.value)
    (draw_vertex_children vertex)
    (vertex_color graph cursor vertex)
    Color.(
      if Old_Graph.parent_edges graph vertex |> Old_Edge.Set.cardinal < 2 then
        black
      else orange)

let draw_edge (graph : Old_Graph.t) (live : Old_Edge.Set.t) (edge : Old_Edge.t)
    : string =
  let source_id = Uuid.Id.to_string edge.value.source.vertex.id in
  let target_id = Uuid.Id.to_string edge.value.target.id in
  let edge_id = Uuid.Id.to_string edge.id in
  let position = Lang.Position.show edge.value.source.position in
  let field = Lang.Position.short_name edge.value.source.position in
  let color =
    let num_conflicts =
      Old_Edge.Set.(
        remove edge live
        |> filter (fun (e : Old_Edge.t) -> e.value.source = edge.value.source)
        |> cardinal)
    in
    let num_parents =
      Old_Edge.Set.cardinal (Old_Graph.parent_edges graph edge.value.target)
    in
    if num_conflicts = 0 && num_parents = 1 then Color.black
    else if num_conflicts > 0 then Color.red
    else Color.orange
  in
  Format.sprintf
    {|n%s:%s -> n%s [color=%s,label="%s",edgeURL="#",edgetooltip="id: %s\nsource: %s\nposition: %s\ntarget: %s",labeltooltip="id: %s\nsource: %s\nposition: %s\ntarget: %s"]|}
    source_id field target_id color edge_id edge_id source_id position target_id
    edge_id source_id position target_id

let maybe_draw_cursor_hole (graph : Old_Graph.t) (cursor : Cursor.t) :
    string list * string list =
  if
    Old_Edge.Set.is_empty
      (Old_Graph.child_edges graph cursor.vertex cursor.position)
  then
    ( [
        Format.sprintf
          {|hole [label="",shape=circle,style=filled,fillcolor=%s]|}
          Color.purple;
      ],
      [
        Format.sprintf "n%s:%s -> hole"
          (Uuid.Id.to_string cursor.vertex.id)
          (Lang.Position.short_name cursor.position);
      ] )
  else ([], [])

let draw_graph (graph : Old_Graph.t) (cursor : Cursor.t) : string =
  let nodes =
    Old_Graph.vertexes graph |> Old_Vertex.Set.elements
    |> List.map (draw_vertex graph cursor)
  in
  let edges =
    let live = Old_Graph.live_edges graph in
    live |> Old_Edge.Set.elements |> List.map (draw_edge graph live)
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
