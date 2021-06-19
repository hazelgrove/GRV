open OptionUtil.Syntax

module type S = sig
  include Set.S

  val construct_map :
    (elt -> Id.Gen.t -> (GraphAction.t list * Id.Gen.t) option) ->
    t ->
    Id.Gen.t ->
    (GraphAction.t list * Id.Gen.t) option
end

module Make (T : Set.OrderedType) = struct
  include Set.Make (T)

  let construct_map
      (f : elt -> Id.Gen.t -> (GraphAction.t list * Id.Gen.t) option)
      (conflict : t) (u_gen : Id.Gen.t) : (GraphAction.t list * Id.Gen.t) option
      =
    elements conflict
    |> List.fold_left
         (fun opt elt ->
           let* acc, u_gen = opt in
           let+ acc', u_gen = f elt u_gen in
           (acc @ acc', u_gen))
         (Some ([], u_gen))
end
