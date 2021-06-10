type t = {
  id : Id.t;
  source : Vertex.t;
  position : GroveLang.position;
  target : Vertex.t;
}

module OrderedType = struct
  type nonrec t = t

  let compare = compare
end

module Set = struct
  include Set.Make (OrderedType)

  let targets (edges : t) : Vertex.Set.t =
    elements edges |> List.map (fun edge -> edge.target) |> Vertex.Set.of_list
end

module Map = struct
  include Map.Make (OrderedType)

  let keys (map : EdgeState.t t) : Set.t =
    bindings map |> List.map fst |> Set.of_list

  let of_list (bindings : (key * 'a) list) : 'a t =
    of_seq (List.to_seq bindings)
end
