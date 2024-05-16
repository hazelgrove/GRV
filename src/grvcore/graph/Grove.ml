type t = {
  multiparented : Tree.t list;
  deleted : Tree.t list;
  reachable : Tree.t;
  wreaths : Tree.t list; (* unicycles *)
}

type in_degree = One | Many

let push_edge (vertex : Vertex.t) (edge : Edge.t) :
    Edge.Set.t Vertex.Map.t -> Edge.Set.t Vertex.Map.t =
  Vertex.Map.update vertex (function
    | None -> Some (Edge.Set.singleton edge)
    | Some edges -> Some (Edge.Set.add edge edges))

(* vertices with no live (parents|children) map to None *)
let adjacency_maps (live_edges : Edge.Set.t) :
    Edge.Set.t Vertex.Map.t * Edge.Set.t Vertex.Map.t =
  (Vertex.Map.empty, Vertex.Map.empty)
  |> Edge.Set.fold
       (fun edge (parents, children) ->
         ( push_edge edge.value.target edge parents,
           push_edge edge.value.source.vertex edge children ))
       live_edges

(* vertices with in-degree 0 map to None *)
let in_degree_map (live_edges : Edge.Set.t) : in_degree Vertex.Map.t =
  Vertex.Map.empty
  |> Edge.Set.fold
       (fun edge map ->
         map
         |> Vertex.Map.update edge.value.target (function
              | None -> Some One
              | Some (One | Many) -> Some Many))
       live_edges

(* categorizes vertices based on in_degree *)
let push_vertex (vertex : Vertex.t)
    ((multiv, univ, av) : Vertex.Set.t * Vertex.Set.t * Vertex.Set.t)
    (in_degrees : in_degree Vertex.Map.t) :
    Vertex.Set.t * Vertex.Set.t * Vertex.Set.t =
  match Vertex.Map.find_opt vertex in_degrees with
  | Some Many -> (Vertex.Set.add vertex multiv, univ, av)
  | Some One -> (multiv, Vertex.Set.add vertex univ, av)
  | None -> (multiv, univ, Vertex.Set.add vertex av)

(* all edges -> vertex sets *)
(* uses push_vertex to partition *)
let partition_vertexes (all_edges : Edge.Set.t)
    (in_degrees : in_degree Vertex.Map.t) :
    Vertex.Set.t * Vertex.Set.t * Vertex.Set.t =
  (Vertex.Set.empty, Vertex.Set.empty, Vertex.Set.empty)
  |> Edge.Set.fold
       (fun edge (multiv, univ, av) ->
         let multiv, univ, av =
           push_vertex edge.value.target (multiv, univ, av) in_degrees
         in
         push_vertex edge.value.source.vertex (multiv, univ, av) in_degrees)
       all_edges

(* vertex -> tree *)
let rec traverse_vertex ?(seen : Vertex.Set.t = Vertex.Set.empty)
    ?(remaining : Vertex.Set.t = Vertex.Set.empty) (vertex : Vertex.t)
    (children : Edge.Set.t Vertex.Map.t) : Tree.t * Vertex.Set.t * Vertex.Set.t
    =
  if Vertex.Set.mem vertex seen then (Ref vertex, seen, remaining)
  else
    let seen = Vertex.Set.add vertex seen in
    let remaining = Vertex.Set.remove vertex remaining in
    let edges =
      Vertex.Map.find_opt vertex children
      |> Option.value ~default:Edge.Set.empty
    in
    let children, seen, remaining =
      traverse_edges edges children ~seen ~remaining
    in
    (Vertex (vertex, children), seen, remaining)

(* edges -> position map *)
and traverse_edges ~(seen : Vertex.Set.t) ~(remaining : Vertex.Set.t)
    (edges : Edge.Set.t) (children : Edge.Set.t Vertex.Map.t) :
    Tree.children Position_map.t * Vertex.Set.t * Vertex.Set.t =
  Edge.Set.elements edges
  |> List.fold_left
       (fun (tree_children, seen, remaining) (edge : Edge.t) ->
         let tree, seen, remaining =
           traverse_vertex edge.value.target children ~seen ~remaining
         in
         let tree_children =
           Position_map.push edge.value.source.position
             (Tree.child edge.id tree) tree_children
         in
         (tree_children, seen, remaining))
       (Position_map.empty, seen, remaining)

(* vertices -> trees *)
let rec traverse_vertexes ~(seen : Vertex.Set.t) ~(remaining : Vertex.Set.t)
    (vertexes : Vertex.Set.t) (children : Edge.Set.t Vertex.Map.t) :
    Tree.t list * Vertex.Set.t * Vertex.Set.t =
  match Vertex.Set.choose_opt vertexes with
  | None -> ([], seen, remaining)
  | Some vertex ->
      let tree, seen, remaining =
        traverse_vertex vertex children ~seen ~remaining
      in
      let roots = Vertex.Set.remove vertex vertexes in
      let trees, seen, remaining =
        traverse_vertexes roots children ~seen ~remaining
      in
      (tree :: trees, seen, remaining)

(* vertex -> vertex *)
let rec walk_up ~(seen : Vertex.Set.t) (vertex : Vertex.t)
    (parents : Edge.Set.t Vertex.Map.t) : Vertex.t =
  if Vertex.Set.mem vertex seen then vertex
  else
    match Vertex.Map.find_opt vertex parents with
    | None -> assert false
    | Some edges ->
        assert (Edge.Set.cardinal edges = 1);
        let edge = Edge.Set.min_elt edges in
        let seen = Vertex.Set.add vertex seen in
        walk_up edge.value.source.vertex parents ~seen

(* vertices -> trees *)
(* unicyle traversal *)
let rec wreath_traverse ~(seen : Vertex.Set.t) (remaining : Vertex.Set.t)
    (parents : Edge.Set.t Vertex.Map.t) (children : Edge.Set.t Vertex.Map.t) :
    Tree.t list =
  if Vertex.Set.is_empty remaining then []
  else
    (* Find vertex with lowest ID *)
    let v0 = Vertex.Set.min_elt remaining in
    let root = walk_up v0 parents ~seen:remaining in
    let tree, seen, remaining =
      traverse_vertex root children ~seen ~remaining
    in
    tree :: wreath_traverse remaining parents children ~seen

let decompose (graph : Graph.t) : t * Edge.Set.t Vertex.Map.t =
  let all_edges = Graph.edges graph in
  let live_edges = Graph.live_edges graph in
  let parents, children = adjacency_maps live_edges in
  let in_degrees = in_degree_map live_edges in
  let multiv, univ, av = partition_vertexes all_edges in_degrees in
  let av = Vertex.Set.remove Vertex.root av in
  let multiparented, seen, remaining =
    traverse_vertexes multiv children ~seen:Vertex.Set.empty ~remaining:univ
  in
  let deleted, seen, remaining =
    traverse_vertexes av children ~seen ~remaining
  in
  let reachable, seen, remaining =
    traverse_vertex Vertex.root children ~seen ~remaining
  in
  let wreaths = wreath_traverse remaining parents children ~seen in
  ({ multiparented; deleted; reachable; wreaths }, children)

(* Unit Tests *)

let%test_module "Graph.decompose" =
  (module struct
    let print_results = true

    let report_tree (prefix : string) (tree : Tree.t) : unit =
      print_string ("\n" ^ prefix ^ ": " ^ Tree.to_string tree)

    let report_trees (prefix : string) (trees : Tree.t list) : unit =
      print_string ("\n" ^ prefix ^ ":");
      List.iter (fun tree -> print_string ("\n  " ^ Tree.to_string tree)) trees

    let check_decompose ?(multiparented : Tree.t list = [])
        ?(deleted : Tree.t list = [])
        ?(reachable : Tree.t = Vertex (Vertex.root, Position_map.empty))
        ?(wreaths : Tree.t list = []) (graph : Graph.t) : bool =
      let got, _ = decompose graph in
      let want = { multiparented; deleted; reachable; wreaths } in
      got = want
      ||
      (if print_results then (
         print_string "\n\n--\nGOT:";
         report_trees "MP" got.multiparented;
         report_trees "D" got.deleted;
         report_tree "R" got.reachable;
         report_trees "SC" got.wreaths;
         print_string "\n\nWANT:";
         report_trees "MP" multiparented;
         report_trees "D" deleted;
         report_tree "R" reachable;
         report_trees "SC" wreaths);
       false)

    let v0 = Vertex.root
    let v1 = Vertex.mk Exp_plus
    let v2 = Vertex.mk Exp_times
    let e01 = Edge.mk Cursor.{ vertex = v0; position = Root_root_root } v1
    let e12 = Edge.mk Cursor.{ vertex = v1; position = Exp_plus_left } v2
    let e12' = Edge.mk Cursor.{ vertex = v1; position = Exp_plus_right } v2
    let e21 = Edge.mk Cursor.{ vertex = v2; position = Exp_times_left } v1
    let%test "empty graph" = check_decompose Graph.empty

    let%test_module "one vertex" =
      (module struct
        (* well-sorted multiparented and wreaths are impossible *)

        let%test "deleted" =
          check_decompose
            Graph.(empty |> add e01 Deleted)
            ~deleted:[ Tree.vertex v1 [] ]

        let%test "reachable" =
          check_decompose
            Graph.(empty |> add e01 Created)
            ~reachable:
              (Tree.vertex v0
                 [ (Root_root_root, [ (e01.id, Tree.vertex v1 []) ]) ])
      end)

    let%test_module "two vertexes" =
      (module struct
        let%test "multiparented" =
          check_decompose
            Graph.(
              empty |> add e01 Created |> add e12 Created |> add e12' Created)
            ~multiparented:[ Tree.vertex v2 [] ]
            ~reachable:
              (Tree.vertex v0
                 [
                   ( Root_root_root,
                     [
                       ( e01.id,
                         Tree.vertex v1
                           [
                             (Exp_plus_left, [ (e12.id, Ref v2) ]);
                             (Exp_plus_right, [ (e12'.id, Ref v2) ]);
                           ] );
                     ] );
                 ])

        let%test "deleted e01" =
          check_decompose
            Graph.(empty |> add e01 Deleted |> add e12 Created)
            ~deleted:
              [
                Tree.vertex v1
                  [ (Exp_plus_left, [ (e12.id, Tree.vertex v2 []) ]) ];
              ]

        let%test "reachable e01 deleted e12" =
          check_decompose
            Graph.(empty |> add e01 Created |> add e12 Deleted)
            ~deleted:[ Tree.vertex v2 [] ]
            ~reachable:
              (Tree.vertex v0
                 [ (Root_root_root, [ (e01.id, Tree.vertex v1 []) ]) ])

        let%test "reachable e01 e12" =
          check_decompose
            Graph.(empty |> add e01 Created |> add e12 Created)
            ~reachable:
              (Tree.vertex v0
                 [
                   ( Root_root_root,
                     [
                       ( e01.id,
                         Tree.vertex v1
                           [ (Exp_plus_left, [ (e12.id, Tree.vertex v2 []) ]) ]
                       );
                     ] );
                 ])

        let%test "wreaths" =
          check_decompose
            Graph.(
              empty |> add e01 Deleted |> add e12 Created |> add e21 Created)
            ~wreaths:
              [
                Tree.vertex v1
                  [
                    ( Exp_plus_left,
                      [
                        ( e12.id,
                          Tree.vertex v2
                            [ (Exp_times_left, [ (e21.id, Ref v1) ]) ] );
                      ] );
                  ];
              ]
      end)
  end)
