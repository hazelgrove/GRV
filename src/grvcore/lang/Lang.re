type show('position) =
  | Str(string)
  | Pos('position);

module type T = {
  module Sort: {
    type t;
    let show: t => string;
  };

  module Position: {
    type t;
    let show: t => string;
    let sort: t => Sort.t;
  };

  module Constructor: {
    type t;
    let sort: t => Sort.t;
    let arity: t => list((Position.t, Sort.t));
    let show: t => string;
    let initial_position: t => option(Position.t);
    let wrap_position: t => option(Position.t);
  };

  module Root: {
    let sort: Sort.t;
    let constructor: Constructor.t;
    let position: Position.t;
  };

  module Show: {
    type t = show(Position.t);
    let position: Position.t => list(t);
    let constructor: Constructor.t => list(t);
  };
};

module type S = {
  include T;
  module Html: {
    open Virtual_dom.Vdom;
    let position: Position.t => Node.t;
    let constructor: Constructor.t => Node.t;
  };
};

module Make = (L: T) : S => {
  include L;

  module Show = {
    open Virtual_dom.Vdom;
    open Attr;
    open Node;

    let show: show => Node.t =
      fun
      | Str(str) => text(str)
      | Pos(position) => {
          let sort = position |> Position.sort |> Sort.show;
          let position = position |> Position.show;
          let attrs = [classes([sort, position])];
          (??);
        };

    let position = (position: Position.t): Node.t => {
      let attrs = [classes([Position.show(position)])];
      let content = L.Show.position(position) |> List.map(show);
      ();
    };

    let constructor: Constructor.t => Node.t = {
      (??);
    };
  };
};
