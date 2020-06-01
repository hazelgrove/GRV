let rec mark_reachable (graph : Graph.t) (seen : Vertex.Set.t ref)
    (vertex : Vertex.t) : unit =
  if Vertex.Set.mem vertex !seen then ()
  else (
    seen := Vertex.Set.add vertex !seen;
    List.iter
      (fun index ->
        Edge.Set.iter
          (fun edge -> mark_reachable graph seen edge.value.target)
          (Graph.children (Cursor.mk vertex index) graph))
      (Lang.Index.child_indexes vertex.value) )

let add (graph : Graph.t) (seen : Vertex.Set.t ref) (roots : Vertex.Set.t ref)
    (vertex : Vertex.t) : unit =
  roots := Vertex.Set.add vertex !roots;
  mark_reachable graph seen vertex

let add_if_orphan (graph : Graph.t) (seen : Vertex.Set.t ref)
    (roots : Vertex.Set.t ref) (vertex : Vertex.t) : unit =
  if Edge.Set.is_empty (Graph.parents vertex graph) then
    add graph seen roots vertex
  else ()

let add_if_unseen (graph : Graph.t) (seen : Vertex.Set.t ref)
    (roots : Vertex.Set.t ref) (vertex : Vertex.t) : unit =
  if Vertex.Set.mem vertex !seen then () else add graph seen roots vertex

let roots (graph : Graph.t) : Vertex.Set.t =
  let seen = ref Vertex.Set.empty in
  let roots = ref Vertex.Set.empty in

  (* Add orphans.

     Since nothing can point to Vertex.root, this adds Vertex.root.
  *)
  Uuid.Map.iter
    (fun _key -> add_if_orphan graph seen roots)
    graph.cache.vertexes;
  (* Add oldest elements of cycles.

     At this point, the only unseen vertexes are the ones in strongly connected
     components (SCCs).

     Thus, picking an arbitrary vertex and marking the vertexes reachable from
     that vertex will pick out an entire SCC.

     Since `iter` goes in increasing order of keys, this always picks the oldest
     element of an SCC.
  *)
  Uuid.Map.iter
    (fun _key -> add_if_unseen graph seen roots)
    graph.cache.vertexes;
  !roots
