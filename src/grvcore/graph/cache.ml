type t = {
  (* Contains both Created and Destroyed *)
  vertices : Vertex.t Uuid.Map.t;
  (* TODO: rename to vertexes *)
  edges : Edge.t Uuid.Map.t;
  (* Contains only Created *)
  parents : Edge.Set.t Vertex.Map.t;
  children : Edge.Set.t Cursor.Map.t;
}

let mk vertices edges parents children : t =
  { vertices; edges; parents; children }

let empty : t =
  let vertices = Uuid.Map.singleton Vertex.root.id Vertex.root in
  let edges = Uuid.Map.empty in
  let parents = Vertex.Map.empty in
  let children = Cursor.Map.empty in
  mk vertices edges parents children

let vertex (vertex : Vertex.t) (cache : t) : Vertex.t =
  Uuid.Map.find vertex.id cache.vertices

let parents (vertex : Vertex.t) ?(filter : (Edge.t -> bool) option) (cache : t)
    : Edge.Set.t =
  Edge.Set.filter (Option.value filter ~default:(fun _ -> true))
  @@ Option.value
       (Vertex.Map.find_opt vertex cache.parents)
       ~default:Edge.Set.empty

let children (cursor : Cursor.t) ?(filter : (Edge.t -> bool) option) (cache : t)
    : Edge.Set.t =
  Edge.Set.filter (Option.value filter ~default:(fun _ -> true))
  @@ Option.value
       (Cursor.Map.find_opt cursor cache.children)
       ~default:Edge.Set.empty

let pp (fmt : Format.formatter) (cache : t) : unit =
  let open Format in
  fprintf fmt "Vertices\n";
  Uuid.Map.iter (fun _ v -> fprintf fmt "%a\n" Vertex.pp v) cache.vertices;
  fprintf fmt "\nEdges\n";
  Uuid.Map.iter
    (fun id e -> fprintf fmt "%s = %a\n" (Uuid.Id.show id) Edge.pp e)
    cache.edges

(* TODO: review this carefully (note only parents/children maps updated) *)
let connect_parents (edge : Edge.t) (cache : t) : t =
  let edges = Uuid.Map.add edge.id edge cache.edges in
  let source = (Edge.source edge).vertex in
  let target = Edge.target edge in
  let vertices =
    cache.vertices
    |> Uuid.Map.add source.id source
    |> Uuid.Map.add target.id target
  in
  let parents = parents target cache in
  let parents =
    Vertex.Map.add target (Edge.Set.add edge parents) cache.parents
  in
  { cache with vertices; edges; parents }

(* Note that this disconnects *all* parents of edge.target *)
let disconnect_parents (edge : Edge.t) (cache : t) : t =
  let edges = Uuid.Map.add edge.id edge cache.edges in
  let source = (Edge.source edge).vertex in
  let target = Edge.target edge in
  let vertices =
    cache.vertices
    |> Uuid.Map.add source.id source
    |> Uuid.Map.add target.id target
  in
  let parents = parents target cache in
  let parents =
    Vertex.Map.add target
      (* TODO: is there a bug here? *)
      (Edge.Set.filter (Edge.equal edge) parents)
      cache.parents
  in
  { cache with vertices; edges; parents }

let connect_children (edge : Edge.t) (cache : t) : t =
  let edges = Uuid.Map.add edge.id edge cache.edges in
  let cursor = Edge.source edge in
  let source = cursor.vertex in
  let target = Edge.target edge in
  let vertices =
    cache.vertices
    |> Uuid.Map.add source.id source
    |> Uuid.Map.add target.id target
  in
  let children = children cursor cache in
  let children =
    Cursor.Map.add cursor (Edge.Set.add edge children) cache.children
  in
  { cache with vertices; edges; children }

(* Note that this disconnects *all* children of edge.source *)
let disconnect_children (edge : Edge.t) (cache : t) : t =
  let edges = Uuid.Map.add edge.id edge cache.edges in
  let cursor = Edge.source edge in
  let source = cursor.vertex in
  let target = Edge.target edge in
  let vertices =
    cache.vertices
    |> Uuid.Map.add source.id source
    |> Uuid.Map.add target.id target
  in
  let children = children cursor cache in
  let children =
    Cursor.Map.add cursor
      (Edge.Set.filter (Edge.equal edge) children)
      cache.children
  in
  { cache with vertices; edges; children }
