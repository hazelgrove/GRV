type t = {
  vertices : Vertex.t Uuid.Map.t;
  parents : Edge.Set.t Vertex.Map.t;
  children : Edge.Set.t Cursor.Map.t;
}

let mk vertices parents children : t = { vertices; parents; children }

let empty : t =
  let vertices = Uuid.Map.singleton Vertex.root.id Vertex.root in
  let parents = Vertex.Map.empty in
  let children = Cursor.Map.empty in
  mk vertices parents children

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
  Uuid.Map.iter (fun _ v -> fprintf fmt "%a\n" Vertex.pp v) cache.vertices

(* TODO: review this carefully (note only parents/children maps updated) *)
let connect_parents (edge : Edge.t) (cache : t) : t =
  let target = Edge.target edge in
  let parents = parents target cache in
  let parents =
    Vertex.Map.add target (Edge.Set.add edge parents) cache.parents
  in
  { cache with parents }

let disconnect_parents (edge : Edge.t) (cache : t) : t =
  let target = Edge.target edge in
  let parents = parents target cache in
  let parents =
    Vertex.Map.add target
      (Edge.Set.filter (Edge.equal edge) parents)
      cache.parents
  in
  { cache with parents }

let connect_children (edge : Edge.t) (cache : t) : t =
  let source = Edge.source edge in
  let children = children source cache in
  let children =
    Cursor.Map.add source (Edge.Set.add edge children) cache.children
  in
  { cache with children }

let disconnect_children (edge : Edge.t) (cache : t) : t =
  let source = Edge.source edge in
  let children = children source cache in
  let children =
    Cursor.Map.add source
      (Edge.Set.filter (Edge.equal edge) children)
      cache.children
  in
  { cache with children }
