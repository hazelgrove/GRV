module Instance = struct
  type t' = {
    graph : Graph.t;
    cursor : Cursor.t;
    actions : Graph_action.t list;
  }

  type t = t' Uuid.Wrap.t

  let cutoff (m1 : t) (m2 : t) : bool = m1 == m2

  let mk () : t =
    Uuid.Wrap.mk { graph = Graph.empty; cursor = Cursor.root; actions = [] }

  module OrderedType = struct
    type nonrec t = t

    let compare (i1 : t) (i2 : t) : int = Uuid.Id.compare i1.id i2.id
  end
end

type t = Instance.t Uuid.Map.t

let empty : t =
  let inst1 = Instance.mk () in
  let inst2 = Instance.mk () in
  (* TODO: helper for this *)
  Uuid.Map.of_seq (List.to_seq [ (inst1.id, inst1); (inst2.id, inst2) ])

let cutoff (m1 : t) (m2 : t) : bool = m1 == m2
