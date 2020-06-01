module Instance = struct
  type t = {
    id : int;
    graph : Graph.t;
    cursor : Cursor.t;
    actions : Graph_action.t list;
  }

  let cutoff (m1 : t) (m2 : t) : bool = m1 == m2

  let empty (id : int) : t =
    { id; graph = Graph.empty; cursor = Cursor.root; actions = [] }
end

include Util.Int.Map

type t = Instance.t Util.Int.Map.t

let empty =
  Util.Int.Map.of_seq
    (List.to_seq [ (0, Instance.empty 0); (1, Instance.empty 1) ])

let cutoff (m1 : t) (m2 : t) : bool = m1 == m2
