type t = Edge_state.t Edge.Map.t

let empty : t = Edge.Map.empty

let apply_action (graph : t) (action : Graph_action.t) : t =
  let old_state = Edge.Map.find_opt action.edge graph in
  let new_state = action.state in
  match (old_state, new_state) with
  | Some Deleted, _ -> graph
  | Some Created, Created -> graph
  | (Some Created | None), Deleted ->
      Edge.Map.add action.edge Edge_state.Deleted graph
  | None, Created ->
      (* TODO: assert not already exists? *)
      (* TODO: short circuit if deleting a non-existant *)
      Edge.Map.add action.edge Edge_state.Created graph

(* Edge Queries *)

let edges (graph : t) : Edge.Set.t =
  Edge.Map.bindings graph |> List.map fst |> Edge.Set.of_list

let live_edges (graph : t) : Edge.Set.t =
  Edge.Map.bindings graph
  |> List.filter Edge_state.(function _, Created -> true | _ -> false)
  |> List.map fst |> Edge.Set.of_list

let parent_edges (graph : t) (vertex : Vertex.t) : Edge.Set.t =
  live_edges graph |> Edge.Set.filter (fun edge -> edge.value.target = vertex)

let child_edges (graph : t) (vertex : Vertex.t) (position : Lang.Position.t) :
    Edge.Set.t =
  live_edges graph
  |> Edge.Set.filter (fun edge ->
         edge.value.source = Cursor.{ vertex; position })

(* Vertex Queries *)

let (vertexes, live_vertexes) : (t -> Vertex.Set.t) * (t -> Vertex.Set.t) =
  let impl (edge_source : t -> Edge.Set.t) : t -> Vertex.Set.t =
   fun graph ->
    Vertex.Set.empty
    |> Edge.Set.fold
         (fun edge vertexes ->
           vertexes
           |> Vertex.Set.add edge.value.source.vertex
           |> Vertex.Set.add edge.value.target)
         (edge_source graph)
  in
  (impl edges, impl live_edges)

let parent_vertexes (graph : t) (vertex : Vertex.t) : Vertex.Set.t =
  Vertex.Set.empty
  |> Edge.Set.fold
       (fun e -> Vertex.Set.add e.value.source.vertex)
       (parent_edges graph vertex)

let orphaned (graph : t) : Vertex.Set.t =
  vertexes graph
  |> Edge.Set.fold
       (fun edge -> Vertex.Set.remove edge.value.target)
       (live_edges graph)

let deleted (graph : t) : Vertex.Set.t =
  orphaned graph |> Vertex.Set.remove Vertex.root

let multiparented (graph : t) : Vertex.Set.t =
  (* find them all in one pass *)
  Vertex.Map.empty
  |> Edge.Set.fold
       (fun edge ->
         Vertex.Map.update edge.value.target (function
           | None -> Some 1
           | Some _ -> Some 2))
       (live_edges graph)
  |> Vertex.Map.filter (fun _ count -> count = 2)
  |> Vertex.Map.bindings |> List.map fst |> Vertex.Set.of_list

let vertex (graph : t) (vertex_id : Uuid.Id.t) : Vertex.t option =
  vertexes graph
  |> Vertex.Set.find_first_opt (fun vertex -> vertex.id = vertex_id)

(* Graph Decomposition *)

type decomp = {
  multiparented : Vertex.Set.t;
  uniparented : Vertex.Set.t;
  deleted : Vertex.Set.t;
  parents : Edge.t list Vertex.Map.t;
  children : Edge.t list Vertex.Map.t;
}

let decompose (graph : t) : decomp =
  let live = live_edges graph in
  (* NOTE: degree-zero vertices are implicitly mapped to None *)
  let degrees =
    Vertex.Map.empty
    |> Edge.Set.fold
         (fun edge map ->
           map
           |> Vertex.Map.update edge.value.target (function
                | None -> Some 1
                | Some _ -> Some 2))
         live
  in
  (* NOTE: orphaned vertices are implicitly mapped to None *)
  let parents =
    Vertex.Map.empty
    |> Edge.Set.fold
         (fun edge ->
           Vertex.Map.update edge.value.target (function
             | None -> Some [ edge ]
             | Some edges -> Some (edge :: edges)))
         live
  in
  (* NOTE: vertices with no children are implicitly mapped to None *)
  let children =
    Vertex.Map.empty
    |> Edge.Set.fold
         (fun edge ->
           Vertex.Map.update edge.value.source.vertex (function
             | None -> Some [ edge ]
             | Some edges -> Some (edge :: edges)))
         live
  in
  {
    multiparented = Vertex.Set.empty;
    uniparented = Vertex.Set.empty;
    deleted = Vertex.Set.empty;
    parents;
    children;
  }
  |> Edge.Set.fold
       (fun edge decomp ->
         match Vertex.Map.find_opt edge.value.target degrees with
         | Some 2 ->
             let multiparented =
               Vertex.Set.add edge.value.target decomp.multiparented
             in
             { decomp with multiparented }
         | Some 1 ->
             let uniparented =
               Vertex.Set.add edge.value.target decomp.uniparented
             in
             { decomp with uniparented }
         | None ->
             let deleted =
               if edge.value.target = Vertex.root then decomp.deleted
               else Vertex.Set.add edge.value.target decomp.deleted
             in
             { decomp with deleted }
         | _ -> assert false)
       live
  |> Edge.Set.fold
       (fun edge decomp ->
         match Vertex.Map.find_opt edge.value.source.vertex degrees with
         | Some 2 ->
             let multiparented =
               Vertex.Set.add edge.value.source.vertex decomp.multiparented
             in
             { decomp with multiparented }
         | Some 1 ->
             let uniparented =
               Vertex.Set.add edge.value.source.vertex decomp.uniparented
             in
             { decomp with uniparented }
         | None ->
             let deleted =
               if edge.value.source.vertex = Vertex.root then decomp.deleted
               else Vertex.Set.add edge.value.source.vertex decomp.deleted
             in
             { decomp with deleted }
         | _ -> assert false)
       live

(* S-Expression Conversions *)

let sexp_of_t (graph : t) : Sexplib.Sexp.t =
  Util.Sexp.of_map (Edge.Map.bindings graph) Edge.sexp_of_t Edge_state.sexp_of_t

let t_of_sexp (sexp : Sexplib.Sexp.t) : t =
  sexp
  |> Sexplib.Std.list_of_sexp (function
       | List [ key_sexp; value_sexp ] ->
           (Edge.t_of_sexp key_sexp, Edge_state.t_of_sexp value_sexp)
       | _ -> failwith __LOC__)
  |> List.to_seq |> Edge.Map.of_seq

(* Unit Tests *)

let print_results = true

let report_set (prefix : string) (set : Vertex.Set.t) : unit =
  print_string ("\n" ^ prefix ^ ":");
  Vertex.Set.iter
    (fun vertex -> print_string (" " ^ Uuid.Id.to_string vertex.id))
    set

let report_map (prefix : string) (iter_prefix : string)
    (iter_id : Edge.t -> Uuid.Id.t) (map : Edge.t list Vertex.Map.t) : unit =
  print_string ("\n" ^ prefix ^ ":");
  Vertex.Map.iter
    (fun vertex edges ->
      print_string (" (" ^ Uuid.Id.to_string vertex.id ^ " " ^ iter_prefix);
      edges
      |> List.iter (fun edge ->
             print_string (" " ^ Uuid.Id.to_string (iter_id edge)));
      print_string ")")
    map

let check_decompose (graph : t) (want : decomp) : bool =
  let ({ multiparented; uniparented; deleted; parents; children } as got) =
    decompose graph
  in
  if print_results && not (got = want) then (
    report_set "MP" multiparented;
    report_set "SP" uniparented;
    report_set "D" deleted;
    report_map "P" "<-" (fun edge -> edge.value.source.vertex.id) parents;
    report_map "C" "->" (fun edge -> edge.value.target.id) children );
  got = want

let%test "decompose empty graph" =
  {
    multiparented = Vertex.Set.empty;
    uniparented = Vertex.Set.empty;
    deleted = Vertex.Set.empty;
    parents = Vertex.Map.empty;
    children = Vertex.Map.empty;
  }
  |> check_decompose empty

let%test "decompose deleted" =
  let v1 = Vertex.mk Lang.Constructor.Exp_plus in
  let v2 = Vertex.mk Lang.Constructor.Exp_times in
  let e12 = Edge.mk Cursor.{ vertex = v1; position = Exp_plus_left } v2 in
  let graph = Edge.Map.(empty |> add e12 Edge_state.Created) in
  let deleted = Vertex.Set.(empty |> add v1) in
  let uniparented = Vertex.Set.(empty |> add v2) in
  let parents = Vertex.Map.(empty |> add v2 [ e12 ]) in
  let children = Vertex.Map.(empty |> add v1 [ e12 ]) in
  { multiparented = Vertex.Set.empty; uniparented; deleted; parents; children }
  |> check_decompose graph

let%test "decompose uniparented" =
  let v1 = Vertex.mk Lang.Constructor.Exp_plus in
  let e1 =
    Edge.mk Cursor.{ vertex = Vertex.root; position = Root_root_root } v1
  in
  let graph = Edge.Map.(add e1 Edge_state.Created empty) in
  let uniparented = Vertex.Set.(empty |> add e1.value.target) in
  let parents = Vertex.Map.(empty |> add e1.value.target [ e1 ]) in
  let children = Vertex.Map.(empty |> add e1.value.source.vertex [ e1 ]) in
  {
    multiparented = Vertex.Set.empty;
    uniparented;
    deleted = Vertex.Set.empty;
    parents;
    children;
  }
  |> check_decompose graph

let%test "decompose multiparented" =
  let v1 = Vertex.mk Lang.Constructor.Exp_plus in
  let e1 =
    Edge.mk Cursor.{ vertex = Vertex.root; position = Root_root_root } v1
  in
  let e1' =
    Edge.mk Cursor.{ vertex = Vertex.root; position = Root_root_root } v1
  in
  let graph =
    Edge.Map.(empty |> add e1 Edge_state.Created |> add e1' Edge_state.Created)
  in
  let multiparented =
    Vertex.Set.(empty |> add e1.value.target |> add e1'.value.target)
  in
  let parents = Vertex.Map.(empty |> add v1 [ e1'; e1 ]) in
  let children = Vertex.Map.(empty |> add Vertex.root [ e1'; e1 ]) in
  {
    multiparented;
    uniparented = Vertex.Set.empty;
    deleted = Vertex.Set.empty;
    parents;
    children;
  }
  |> check_decompose graph
