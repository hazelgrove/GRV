type t = {
  id : Id.t;
  source : Vertex.t;
  position : Position.t;
  target : Vertex.t;
}

module Map = Map.Make (struct
  type t = Old_Edge_state.t

  let compare = compare
end)
