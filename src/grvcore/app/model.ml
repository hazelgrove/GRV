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

type t = Instance.t list

let empty = [ Instance.empty; Instance.empty ]

let cutoff (m1 : t) (m2 : t) : bool = m1 == m2
