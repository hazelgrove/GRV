open OptionUtil.Syntax

type t = {
  noparents : Term.Set.t;
  multiparents : Term.Set.t;
  unicycles : Term.Set.t;
}

let decomp_edge (graph : Graph.t) (edge : Edge.t) : Term.t option =
  match GroveLang.Constructor.sort edge.target.constructor with
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
  let edges_to_term_set (edges : Edge.t list) : Term.Set.t option =
    let+ terms = edges |> List.map (decomp_edge graph) |> OptionUtil.of_list in
    Term.Set.of_list terms
  in
  let noparent_edges, multiparent_edges, unicycle_edges =
    Graph.root_edges graph
  in
  let* noparents = edges_to_term_set noparent_edges in
  let* multiparents = edges_to_term_set multiparent_edges in
  let+ unicycles = edges_to_term_set unicycle_edges in
  { noparents; multiparents; unicycles }

let recomp ({ noparents; multiparents; unicycles } : t) : Graph.t =
  let noparent_bindings =
    Term.Set.elements noparents
    |> List.map Term.recomp |> List.map Graph.bindings |> List.concat
  in
  let multiparent_bindings =
    Term.Set.elements multiparents
    |> List.map Term.recomp |> List.map Graph.bindings |> List.concat
  in
  let unicycle_bindings =
    Term.Set.elements unicycles
    |> List.map Term.recomp |> List.map Graph.bindings |> List.concat
  in
  Graph.of_list (noparent_bindings @ multiparent_bindings @ unicycle_bindings)
