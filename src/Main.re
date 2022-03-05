open Bonsai_web;

// let _: Start.Handle.t(_) =
//   Start.start_standalone(
//     ~initial_input=Id.Gen.init |> Id.Ctx.eval(Editor.init),
//     ~bind_to_element_with_id="container",
//     Widget.editor,
//   );

open CoreImpl;

let _: Start.Handle.t(_) =
  Start.start_standalone(
    ~initial_input=
      Edge.{
        position: Lang.Root.position,
        vertex: Id.Gen.init |> Id.Ctx.(Vertex.init(Root_root) |> eval),
      },
    ~bind_to_element_with_id="container",
    Bonsai.pure(~f=Widget.hole),
  );
