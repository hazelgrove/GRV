type t = Replica.Map.t(int64);

let init = Replica.Map.empty;

let equal = (vc1: t, vc2: t): bool =>
  Replica.Map.equal(Int64.equal, vc1, vc2);

let compare = (vc1: t, vc2: t): int => {
  let flags =
    Replica.Map.merge(
      (_, c1_opt, c2_opt) => {
        Int64.(
          switch (c1_opt, c2_opt) {
          | (Some(c1), Some(c2)) => Some(compare(sub(c1, c2), 0L))
          | (Some(c1), None) => Some(compare(c1, 0L))
          | (None, Some(c2)) => Some(compare(neg(c2), 0L))
          | (None, None) => None
          }
        )
      },
      vc1,
      vc2,
    );
  Replica.Map.cardinal(flags) == 0
    ? 0
    : Replica.Map.for_all((_, flag) => flag == (-1), flags)
        ? (-1) : Replica.Map.for_all((_, flag) => flag == 1, flags) ? 1 : 0;
};

let concurrent = (vc1: t, vc2: t): bool => compare(vc1, vc2) == 0;

let increment = (r: Replica.t, vc: t): t =>
  Replica.Map.update(
    r,
    fun
    | None => Some(1L)
    | Some(c) => Some(Int64.add(c, 1L)),
    vc,
  );

let merge = (vc1: t, vc2: t): t =>
  Replica.Map.union((_, c1, c2) => Some(max(c1, c2)), vc1, vc2);

let to_string = (vc: t): string =>
  Replica.Map.bindings(vc)
  |> List.map(((k, v)) => k ++ "=" ++ Int64.to_string(v))
  |> String.concat("\n");

let%test_module _ =
  (module
   {
     ();
     let vc1: t = init;
     let vc2: t = init;
     let%test _ = compare(vc1, vc2) == 0;
     let%test _ = compare(vc2, vc1) == 0;
     let%test _ = equal(vc1, vc2);
     let%test _ = equal(vc2, vc1);
     let vc1 = increment("R1", vc1);
     let%test _ = compare(vc1, vc2) == 1;
     let%test _ = compare(vc2, vc1) == (-1);
     let%test _ = !equal(vc1, vc2);
     let%test _ = !equal(vc2, vc1);
     let vc1 = increment("R1", vc1);
     let vc1 = increment("R1", vc1);
     let vc2 = increment("R1", vc2);
     let%test _ = compare(vc1, vc2) == 1;
     let%test _ = compare(vc2, vc1) == (-1);
     let%test _ = !equal(vc1, vc2);
     let%test _ = !equal(vc2, vc1);
     let vc1 = increment("R1", vc2);
     let vc2 = increment("R1", vc2);
     let%test _ = compare(vc1, vc2) == 0;
     let%test _ = compare(vc2, vc1) == 0;
     let%test _ = equal(vc1, vc2);
     let%test _ = equal(vc2, vc1);

     let%test_module _ =
       (module
        {
          ();
          let vc1 = increment("R1", vc1);
          let vc2 = increment("R1", vc2);
          let vc1 = increment("R1", vc1);
          let vc2 = increment("R2", vc2);
          let%test _ = compare(vc1, vc2) == 0;
          let%test _ = compare(vc2, vc1) == 0;
          let%test _ = !equal(vc1, vc2);
          let%test _ = !equal(vc2, vc1);
          let vc1 = increment("R1", vc1);
          let%test _ = compare(vc1, vc2) == 0;
          let%test _ = !equal(vc1, vc2);
          let%test _ = !equal(vc2, vc1);
          let vc2 = increment("R2", vc2);
          let vc2 = increment("R2", vc2);
          let vc2 = increment("R2", vc2);
          let%test _ = compare(vc1, vc2) == 0;
          let%test _ = compare(vc2, vc1) == 0;
          let%test _ = !equal(vc1, vc2);
          let%test _ = !equal(vc2, vc1);
          let vc2 = increment("R1", vc2);
          let vc2 = increment("R2", vc2);
          let vc2 = increment("R2", vc2);
          let vc3 = merge(vc1, vc2);
          let%test _ = compare(vc3, vc2) == 0;
          let%test _ = compare(vc3, vc1) == 0;
        });
   });
