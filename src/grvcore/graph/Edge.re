module type T = {
  type position;
  type vertex;

  type source = {
    position,
    vertex,
  };

  type t = {
    id: Id.t,
    source,
    target: vertex,
  };

  module Map: Map.S with type key = t;
  module Set: Set.S with type elt = t;
  // module Map: {
  //   include Map.S with type key = t;
  //   let sexp_of_t: ('a => Sexp.t, t('a)) => Sexp.t;
  //   let t_of_sexp: (Sexp.t => 'a, Sexp.t) => t('a);
  // };
  // module Set: {
  //   include Set.S with type elt = t;
  //   let sexp_of_t: t => Sexp.t;
  //   let t_of_sexp: Sexp.t => t;
  // };
};

module Make =
       (L: Lang.T, V: Vertex.T with type constructor = L.Constructor.t)
       : (T with type position = L.Position.t and type vertex = V.t) => {
  type position = L.Position.t;
  type vertex = V.t;

  type source = {
    position,
    vertex,
  };

  type t = {
    id: Id.t,
    source,
    target: vertex,
  };

  module OrderedType = {
    type nonrec t = t;
    let compare = compare;
  };

  module Map = Map.Make(OrderedType);
  module Set = Set.Make(OrderedType);
  // module Map = {
  //   include Map.Make(OrderedType);
  //   let sexp_of_key: key => Sexp.t = sexp_of_t;
  //   let key_of_sexp: Sexp.t => key = t_of_sexp;
  //   let sexp_of_t = (sexp_of_value: 'a => Sexp.t, edge_map: t('a)): Sexp.t =>
  //     edge_map
  //     |> bindings
  //     |> Std.sexp_of_list(((key, value)) =>
  //          Sexp.List([sexp_of_key(key), sexp_of_value(value)])
  //        );
  //   let t_of_sexp = (value_of_sexp: Sexp.t => 'a, sexp: Sexp.t): t('a) =>
  //     sexp
  //     |> Std.list_of_sexp(
  //          fun
  //          | List([key, value]) => (key_of_sexp(key), value_of_sexp(value))
  //          | sexp' =>
  //            raise(
  //              Conv_error.tuple_of_size_n_expected(
  //                "Edge.t_of_sexp",
  //                2,
  //                sexp',
  //              ),
  //            ),
  //        )
  //     |> List.to_seq
  //     |> of_seq;
  // };
  // module Set = {
  //   include Set.Make(OrderedType);
  //   let sexp_of_elt: elt => Sexp.t = sexp_of_t;
  //   let elt_of_sexp: Sexp.t => elt = t_of_sexp;
  //   let sexp_of_t = (edge_set: t): Sexp.t =>
  //     edge_set |> elements |> Std.sexp_of_list(sexp_of_elt);
  //   let t_of_sexp = (sexp: Sexp.t): t =>
  //     sexp |> Std.list_of_sexp(elt_of_sexp) |> of_list;
  // };
};
