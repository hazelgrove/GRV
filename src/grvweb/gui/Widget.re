open Virtual_dom.Vdom;
open CoreImpl;

module Ctx = {
  [@deriving fields]
  type settings = {verbose: bool};

  module T = {
    type t =
      | Ctx0
      | Ctx(settings);
    // MONOID
    let append = (t1: t, t2: t): t =>
      switch (t1, t2) {
      | (Ctx0, ctx)
      | (_, ctx) => ctx
      };
    let identity: t = Ctx0;
  };

  let defaults: settings = {verbose: true};

  let getter = (f: settings => 'a): (T.t => 'a) =>
    fun
    | Ctx0 => f(defaults)
    | Ctx(settings) => f(settings);

  let verbose: T.t => bool = getter(verbose);

  include Cat.State_Monad.Make((T: Cat.MONOID with type t = T.t));

  type ctx('a) = t('a);

  include Cat.Applicative.Make({
    type t('a) = ctx('a);
    let pure: 'a => t('a) = return;
    let (<*>): (t('a), t('b)) => t(('a, 'b)) =
      (x, y) => {
        let* x = x;
        let* y = y;
        return((x, y));
      };
  });

  include Cat.Functor.Make({
    type t('a) = ctx('a);
    let fmap: (t('a), 'a => 'b) => t('b) =
      (x, f) => {
        let* x = x;
        return(f(x));
      };
  });
};

let sub: Node.node_creator = Node.create("sub");

let muted = (attrs: list(Attr.t), content: list(Node.t)): Node.t =>
  Node.span(Attr.[classes(["muted"]), ...attrs], content);

let hole = (source: Edge.source): Ctx.t(Node.t) => {
  open Ctx;
  let position = source.position |> Lang.Position.show;
  let vertex = source.vertex.id |> Id.to_string;
  let+ verbose = gets(verbose);
  let attrs = Attr.[classes(["Hole"])];
  let content =
    verbose
      ? [muted([], [Node.text("(" ++ position ++ "," ++ vertex ++ ")")])]
      : [];
  Node.span(attrs, content);
  // Ctx.Node.span(
  //   Attr.[classes(["Hole"])],
  //   Node.[
  //     text("□"),
  //     sub(
  //       [],
  //       verbose
  //         ? [muted([], [text("(" ++ position ++ "," ++ vertex ++ ")")])]
  //         : [],
  //     ),
  //   ],
  // );
};

let rec term = (attrs: list(Attr.t)): (Term.t => Node.t) =>
  fun
  | Node(node) => Node.text("N." ++ Id.to_string(node.vertex.id))
  | NodeRef(reftype, edge) =>
    switch (reftype) {
    | MultiParentConflict => Node.text("MP." ++ Id.to_string(edge.id))
    | CycleRoot => Node.text("CR." ++ Id.to_string(edge.id))
    }
  | MultiChildConflict(terms) =>
    terms |> List.map(term(attrs)) |> Node.div(attrs)
  | Hole(source) =>
    Node.text(
      "?."
      ++ Lang.Position.show(source.position)
      ++ "."
      ++ Id.to_string(source.vertex.id),
    );

// let cursor: t(Term.t, Node.t) =
//   term >>| (content => Node.span([], [content]));

// let editor: t(Editor.t, Node.t) = {
//   let f = (editor: Editor.t): Node.t => {
//     Node.text("Editor." ++ Int.to_string(editor.id));
//   };
//   pure(~f);
// };
