open OptionUtil.Syntax

type t = {
  noparents : Term.Set.t;
  multiparents : Term.Set.t;
  unicycles : Term.Set.t;
}

type indegree = Zero | One | Many

let decomp_edge (graph : Graph.t) (edge : Edge.t) : Term.t option =
  match GroveLang.sort_of_constructor edge.target.constructor with
  | Exp ->
      let+ e = Exp.decomp graph edge in
      Term.Exp e
  | Pat ->
      let+ q = Pat.decomp graph edge in
      Term.Pat q
  | Typ ->
      let+ t = Typ.decomp graph edge in
      Term.Typ t

let decomp (graph : Graph.t) : t option =
  let indegrees =
    Graph.bindings graph
    |> List.fold_left
         (fun indegrees ((edge, state) : Graph.binding) ->
           match state with
           | Plus ->
               Vertex.Map.update edge.target
                 (function
                   | None -> Some Zero
                   | Some Zero -> Some One
                   | Some (One | Many) -> Some Many)
                 indegrees
           | Minus -> indegrees)
         Vertex.Map.empty
  in
  let noparent_edges, multiparent_edges, unicycle_edges =
    Graph.edges graph |> Edge.Set.elements
    |> List.fold_left
         (fun (noparent_edges, multiparent_edges, unicycle_edges)
              (edge : Edge.t) ->
           match Vertex.Map.find_opt edge.target indegrees with
           | None -> failwith ("impossible " ^ __LOC__)
           | Some Zero ->
               (edge :: noparent_edges, multiparent_edges, unicycle_edges)
           | Some One ->
               if Graph.is_unicycle_root edge.target graph then
                 (noparent_edges, multiparent_edges, edge :: unicycle_edges)
               else (noparent_edges, multiparent_edges, unicycle_edges)
           | Some Many ->
               (noparent_edges, edge :: multiparent_edges, unicycle_edges))
         ([], [], [])
  in
  let* noparents =
    noparent_edges |> List.map (decomp_edge graph) |> OptionUtil.of_list
  in
  let* multiparents =
    multiparent_edges |> List.map (decomp_edge graph) |> OptionUtil.of_list
  in
  let+ unicycles =
    unicycle_edges |> List.map (decomp_edge graph) |> OptionUtil.of_list
  in
  let noparents = Term.Set.of_list noparents in
  let multiparents = Term.Set.of_list multiparents in
  let unicycles = Term.Set.of_list unicycles in
  { noparents; multiparents; unicycles }

let recomp_term : Term.t -> Graph.t = function
  | Exp e -> Exp.recomp e
  | Pat q -> Pat.recomp q
  | Typ ty -> Typ.recomp ty

let recomp ({ noparents; multiparents; unicycles } : t) : Graph.t =
  let noparent_bindings =
    Term.Set.elements noparents
    |> List.map recomp_term |> List.map Graph.bindings |> List.concat
  in
  let multiparent_bindings =
    Term.Set.elements multiparents
    |> List.map recomp_term |> List.map Graph.bindings |> List.concat
  in
  let unicycle_bindings =
    Term.Set.elements unicycles
    |> List.map recomp_term |> List.map Graph.bindings |> List.concat
  in
  Graph.of_list (noparent_bindings @ multiparent_bindings @ unicycle_bindings)
