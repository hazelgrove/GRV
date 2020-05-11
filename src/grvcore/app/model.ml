module Instance = struct
  type t = {
    graph : Graph.t;
    cursor : Graph.Child.t;
    actions : Graph_action.t list;
  }

  let cutoff (m1 : t) (m2 : t) : bool = m1 == m2

  let empty : t =
    { graph = Graph.empty; cursor = Graph.Child.root; actions = [] }
end

module MapInt = Map.Make (struct
  type t = int

  let compare = Int.compare
end)

type t = Instance.t MapInt.t

let empty =
  MapInt.of_seq (List.to_seq [ (0, Instance.empty); (1, Instance.empty) ])

let cutoff (m1 : t) (m2 : t) : bool = m1 == m2
