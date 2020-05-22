type t = {
  (* Contains both Created and Destroyed *)
  vertexes : Vertex.t Uuid.Map.t;
  edges : Edge.t Uuid.Map.t;
  (* Contains only Created *)
  parents : Edge.Set.t Vertex.Map.t;
  children : Edge.Set.t Cursor.Map.t;
  deleted : Edge.t Option.t;
}

let mk vertexes edges parents children deleted : t =
  { vertexes; edges; parents; children; deleted }

let empty : t =
  let vertexes = Uuid.Map.singleton Vertex.root.id Vertex.root in
  let edges = Uuid.Map.empty in
  let parents = Vertex.Map.empty in
  let children = Cursor.Map.empty in
  mk vertexes edges parents children None

let parents (vertex : Vertex.t) (cache : t) : Edge.Set.t =
  Option.value
    (Vertex.Map.find_opt vertex cache.parents)
    ~default:Edge.Set.empty

let children (cursor : Cursor.t) (cache : t) : Edge.Set.t =
  Option.value
    (Cursor.Map.find_opt cursor cache.children)
    ~default:Edge.Set.empty

let pp (fmt : Format.formatter) (cache : t) : unit =
  let open Format in
  fprintf fmt "vertexes\n";
  Uuid.Map.iter (fun _ v -> fprintf fmt "%a\n" Vertex.pp v) cache.vertexes;
  fprintf fmt "\nEdges\n";
  (* TODO: fix indents of printed edges *)
  Uuid.Map.iter
    (fun id e -> fprintf fmt "%s = %a\n" (Uuid.Id.show id) Edge.pp e)
    cache.edges

(* TODO: review this carefully (note only parents/children maps updated) *)
let create (edge : Edge.t) (cache : t) : t =
  let edges = Uuid.Map.add edge.id edge cache.edges in
  let cursor = Edge.source edge in
  let target = Edge.target edge in
  let source = cursor.vertex in
  let vertexes =
    cache.vertexes
    |> Uuid.Map.add source.id source
    |> Uuid.Map.add target.id target
  in
  let parents = parents target cache in
  let parents =
    Vertex.Map.add target (Edge.Set.add edge parents) cache.parents
  in
  let children = children cursor cache in
  let children =
    Cursor.Map.add cursor (Edge.Set.add edge children) cache.children
  in
  { cache with vertexes; edges; children; parents }

let destroy (edge : Edge.t) (cache : t) : t =
  let edges = Uuid.Map.add edge.id edge cache.edges in
  let cursor = Edge.source edge in
  let target = Edge.target edge in
  let source = cursor.vertex in
  let vertexes =
    cache.vertexes
    |> Uuid.Map.add source.id source
    |> Uuid.Map.add target.id target
  in
  let parents = parents target cache in
  let parents =
    Vertex.Map.add target (Edge.Set.remove edge parents) cache.parents
  in
  let children = children cursor cache in
  let children =
    Cursor.Map.add cursor (Edge.Set.remove edge children) cache.children
  in
  { vertexes; edges; children; parents; deleted = Some edge }
