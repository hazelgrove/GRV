open OptionUtil.Syntax

type t = {
  noparents : Term.Set.t;
  multiparents : Term.Set.t;
  unicycles : Term.Set.t;
}

let rec edecomp (graph : Graph.t) (edge : Edge.t) : Exp.t option =
  match edge.target.constructor with
  | ExpVar x ->
      let+ ingraph = Ingraph.of_vertex edge.target graph in
      Exp.Var (ingraph, x)
  | ExpLam ->
      let* ingraph = Ingraph.of_vertex edge.target graph in
      let* q = pdecomp' edge GroveLang.LamParam graph in
      let* t = tdecomp' edge GroveLang.LamType graph in
      let+ e = edecomp' edge GroveLang.LamBody graph in
      Exp.Lam (ingraph, q, t, e)
  | ExpApp ->
      let* ingraph = Ingraph.of_vertex edge.target graph in
      let* e1 = edecomp' edge GroveLang.AppFun graph in
      let+ e2 = edecomp' edge GroveLang.AppArg graph in
      Exp.App (ingraph, e1, e2)
  | ExpNum n ->
      let+ ingraph = Ingraph.of_vertex edge.target graph in
      Exp.Num (ingraph, n)
  | ExpPlus ->
      let* ingraph = Ingraph.of_vertex edge.target graph in
      let* e1 = edecomp' edge GroveLang.AppFun graph in
      let+ e2 = edecomp' edge GroveLang.AppArg graph in
      Exp.Plus (ingraph, e1, e2)
  | ExpTimes ->
      let* ingraph = Ingraph.of_vertex edge.target graph in
      let* e1 = edecomp' edge GroveLang.AppFun graph in
      let+ e2 = edecomp' edge GroveLang.AppArg graph in
      Exp.Times (ingraph, e1, e2)
  | Root | PatVar _ | TypArrow | TypNum -> None

and pdecomp (graph : Graph.t) (edge : Edge.t) : Pat.t option =
  match edge.target.constructor with
  | PatVar x ->
      let+ ingraph = Ingraph.of_vertex edge.target graph in
      Pat.Var (ingraph, x)
  | Root | ExpVar _ | ExpLam | ExpApp | ExpNum _ | ExpPlus | ExpTimes | TypArrow
  | TypNum ->
      None

and tdecomp (graph : Graph.t) (edge : Edge.t) : Typ.t option =
  match edge.target.constructor with
  | TypArrow ->
      let* ingraph = Ingraph.of_vertex edge.target graph in
      let* ty1 = tdecomp' edge GroveLang.ArrowArg graph in
      let+ ty2 = tdecomp' edge GroveLang.ArrowResult graph in
      Typ.Arrow (ingraph, ty1, ty2)
  | TypNum ->
      let+ ingraph = Ingraph.of_vertex edge.target graph in
      Typ.Num ingraph
  | Root | ExpVar _ | ExpLam | ExpApp | ExpNum _ | ExpPlus | ExpTimes | PatVar _
    ->
      None

and edecomp' (edge : Edge.t) (position : GroveLang.position) (graph : Graph.t) :
    Exp.t option =
  let outedges = Graph.outedges edge.target position graph in
  match Edge.Set.cardinal outedges with
  | 0 -> Some Exp.Hole
  | 1 -> (
      let* outedge = Edge.Set.choose_opt outedges in
      match Vertex.Set.cardinal (Graph.parents outedge.target graph) with
      | 0 -> None
      | _ ->
          let ancestors = Graph.ancestors outedge.target graph in
          let* unicycle_root = Vertex.Set.min_elt_opt ancestors in
          if outedge.target = unicycle_root then Some (Exp.Unicycle edge)
          else edecomp graph edge)
  | _ ->
      let+ children =
        Edge.Set.elements outedges
        |> List.map (edecomp graph)
        |> OptionUtil.of_list
      in
      Exp.Conflict (Exp.C.of_list children)

and pdecomp' (edge : Edge.t) (position : GroveLang.position) (graph : Graph.t) :
    Pat.t option =
  let outedges = Graph.outedges edge.target position graph in
  match Edge.Set.cardinal outedges with
  | 0 -> Some Pat.Hole
  | 1 -> (
      let* outedge = Edge.Set.choose_opt outedges in
      match Vertex.Set.cardinal (Graph.parents outedge.target graph) with
      | 0 -> None
      | _ ->
          let ancestors = Graph.ancestors outedge.target graph in
          let* unicycle_root = Vertex.Set.min_elt_opt ancestors in
          if outedge.target = unicycle_root then Some (Pat.Unicycle edge)
          else pdecomp graph edge)
  | _ ->
      let+ children =
        Edge.Set.elements outedges
        |> List.map (pdecomp graph)
        |> OptionUtil.of_list
      in
      Pat.Conflict (Pat.C.of_list children)

and tdecomp' (edge : Edge.t) (position : GroveLang.position) (graph : Graph.t) :
    Typ.t option =
  let outedges = Graph.outedges edge.target position graph in
  match Edge.Set.cardinal outedges with
  | 0 -> Some Typ.Hole
  | 1 -> (
      let* outedge = Edge.Set.choose_opt outedges in
      match Vertex.Set.cardinal (Graph.parents outedge.target graph) with
      | 0 -> None
      | _ ->
          let ancestors = Graph.ancestors outedge.target graph in
          let* unicycle_root = Vertex.Set.min_elt_opt ancestors in
          if outedge.target = unicycle_root then Some (Typ.Unicycle edge)
          else tdecomp graph edge)
  | _ ->
      let+ children =
        Edge.Set.elements outedges
        |> List.map (tdecomp graph)
        |> OptionUtil.of_list
      in
      Typ.Conflict (Typ.C.of_list children)

let decomp_edge (graph : Graph.t) (edge : Edge.t) : Term.t option =
  match GroveLang.sort_of_constructor edge.target.constructor with
  | Exp ->
      let+ e = edecomp graph edge in
      Term.Exp e
  | Pat ->
      let+ q = pdecomp graph edge in
      Term.Pat q
  | Typ ->
      let+ t = tdecomp graph edge in
      Term.Typ t

type indegree = Zero | One | Many

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
