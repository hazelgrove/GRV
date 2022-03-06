module type T = {
  module Sort: {type t;};

  module Constructor: {
    type t;
    let sort: t => Sort.t;
  };

  module Position: {
    [@deriving enum]
    type t;
    let sort: t => Sort.t;
    let source: t => Constructor.t;
  };
};

module type S = {
  module Sort: {type t;};

  module rec Constructor: {
    type t;
    let sort: t => Sort.t;
    let arity: t => list(Position.t);
  }

  and Position: {
    type t;
    let sort: t => Sort.t;
    let source: t => Constructor.t;
  };
};

module Make = (Lang: T) : S => {
  module Sort = Lang.Sort;

  module rec Constructor:
     {
      type t = Lang.Constructor.t;
      let sort: t => Sort.t;
      let arity: t => list(Position.t);
    } with
      type t = Lang.Constructor.t = {
    type t = Lang.Constructor.t;

    let sort = Lang.Constructor.sort;

    module Map =
      Map.Make({
        type nonrec t = t;
        let compare = compare;
      });

    let arities: Map.t(list(Position.t)) = {
      let indices: list(int) =
        List.init(Lang.Position.max - Lang.Position.min + 1, i => i);
      let positions: list(Lang.Position.t) =
        List.filter_map(Lang.Position.of_enum, indices);
      List.fold_left(
        (arities, position) => {
          let constructor = Position.source(position);
          Map.update(
            constructor,
            fun
            | None => Some([position])
            | Some(positions) => Some([position, ...positions]),
            arities,
          );
        },
        Map.empty,
        positions,
      );
    };

    let arity = (constructor: t): list(Position.t) =>
      Map.find_opt(constructor, arities) |> Option.value(~default=[]);
  }

  and Position: {
    type t = Lang.Position.t;
    let sort: t => Sort.t;
    let source: t => Constructor.t;
  } = {
    type t = Lang.Position.t;
    let sort: t => Sort.t = Lang.Position.sort;
    let source: t => Constructor.t = Lang.Position.source;
  };
};
