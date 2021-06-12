type t = {
  multiparented : Tree.t list;
  deleted : Tree.t list;
  reachable : Tree.t;
  wreaths : Tree.t list;
}

type in_degree = One | Many

let push_edge (vertex : Old_Vertex.t) (edge : Old_Edge.t) :
    Old_Edge.Set.t Old_Vertex.Map.t -> Old_Edge.Set.t Old_Vertex.Map.t =
  Old_Vertex.Map.update vertex (function
    | None -> Some (Old_Edge.Set.singleton edge)
    | Some edges -> Some (Old_Edge.Set.add edge edges))

(* vertices with no live (parents|children) map to None *)
let adjacency_maps (live_edges : Old_Edge.Set.t) :
    Old_Edge.Set.t Old_Vertex.Map.t * Old_Edge.Set.t Old_Vertex.Map.t =
  (Old_Vertex.Map.empty, Old_Vertex.Map.empty)
  |> Old_Edge.Set.fold
       (fun edge (parents, children) ->
         ( push_edge edge.value.target edge parents,
           push_edge edge.value.source.vertex edge children ))
       live_edges

(* vertices with in-degree 0 map to None *)
let in_degree_map (live_edges : Old_Edge.Set.t) : in_degree Old_Vertex.Map.t =
  Old_Vertex.Map.empty
  |> Old_Edge.Set.fold
       (fun edge map ->
         map
         |> Old_Vertex.Map.update edge.value.target (function
              | None -> Some One
              | Some (One | Many) -> Some Many))
       live_edges

let push_vertex (vertex : Old_Vertex.t)
    ((multiv, univ, av) :
      Old_Vertex.Set.t * Old_Vertex.Set.t * Old_Vertex.Set.t)
    (in_degrees : in_degree Old_Vertex.Map.t) :
    Old_Vertex.Set.t * Old_Vertex.Set.t * Old_Vertex.Set.t =
  match Old_Vertex.Map.find_opt vertex in_degrees with
  | Some Many -> (Old_Vertex.Set.add vertex multiv, univ, av)
  | Some One -> (multiv, Old_Vertex.Set.add vertex univ, av)
  | None -> (multiv, univ, Old_Vertex.Set.add vertex av)

(* edges -> vertex sets *)
let partition_vertexes (all_edges : Old_Edge.Set.t)
    (in_degrees : in_degree Old_Vertex.Map.t) :
    Old_Vertex.Set.t * Old_Vertex.Set.t * Old_Vertex.Set.t =
  (Old_Vertex.Set.empty, Old_Vertex.Set.empty, Old_Vertex.Set.empty)
  |> Old_Edge.Set.fold
       (fun edge (multiv, univ, av) ->
         let multiv, univ, av =
           push_vertex edge.value.target (multiv, univ, av) in_degrees
         in
         push_vertex edge.value.source.vertex (multiv, univ, av) in_degrees)
       all_edges

(* vertex -> tree *)
let rec traverse_vertex ?(seen : Old_Vertex.Set.t = Old_Vertex.Set.empty)
    ?(remaining : Old_Vertex.Set.t = Old_Vertex.Set.empty)
    (vertex : Old_Vertex.t) (children : Old_Edge.Set.t Old_Vertex.Map.t) :
    Tree.t * Old_Vertex.Set.t * Old_Vertex.Set.t =
  if Old_Vertex.Set.mem vertex seen then (Ref vertex, seen, remaining)
  else
    let seen = Old_Vertex.Set.add vertex seen in
    let remaining = Old_Vertex.Set.remove vertex remaining in
    let edges =
      Old_Vertex.Map.find_opt vertex children
      |> Option.value ~default:Old_Edge.Set.empty
    in
    let children, seen, remaining =
      traverse_edges edges children ~seen ~remaining
    in
    (Vertex (vertex, children), seen, remaining)

(* edges -> position map *)
and traverse_edges ~(seen : Old_Vertex.Set.t) ~(remaining : Old_Vertex.Set.t)
    (edges : Old_Edge.Set.t) (children : Old_Edge.Set.t Old_Vertex.Map.t) :
    Tree.children Position_map.t * Old_Vertex.Set.t * Old_Vertex.Set.t =
  Old_Edge.Set.elements edges
  |> List.fold_left
       (fun (tree_children, seen, remaining) (edge : Old_Edge.t) ->
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
let rec traverse_vertexes ~(seen : Old_Vertex.Set.t)
    ~(remaining : Old_Vertex.Set.t) (vertexes : Old_Vertex.Set.t)
    (children : Old_Edge.Set.t Old_Vertex.Map.t) :
    Tree.t list * Old_Vertex.Set.t * Old_Vertex.Set.t =
  match Old_Vertex.Set.choose_opt vertexes with
  | None -> ([], seen, remaining)
  | Some vertex ->
      let tree, seen, remaining =
        traverse_vertex vertex children ~seen ~remaining
      in
      let roots = Old_Vertex.Set.remove vertex vertexes in
      let trees, seen, remaining =
        traverse_vertexes roots children ~seen ~remaining
      in
      (tree :: trees, seen, remaining)

(* vertex -> vertex *)
let rec walk_up ~(seen : Old_Vertex.Set.t) (vertex : Old_Vertex.t)
    (parents : Old_Edge.Set.t Old_Vertex.Map.t) : Old_Vertex.t =
  if Old_Vertex.Set.mem vertex seen then vertex
  else
    match Old_Vertex.Map.find_opt vertex parents with
    | None -> assert false
    | Some edges ->
        assert (Old_Edge.Set.cardinal edges = 1);
        let edge = Old_Edge.Set.min_elt edges in
        let seen = Old_Vertex.Set.add vertex seen in
        walk_up edge.value.source.vertex parents ~seen

(* vertices -> trees *)
let rec wreath_traverse ~(seen : Old_Vertex.Set.t)
    (remaining : Old_Vertex.Set.t) (parents : Old_Edge.Set.t Old_Vertex.Map.t)
    (children : Old_Edge.Set.t Old_Vertex.Map.t) : Tree.t list =
  if Old_Vertex.Set.is_empty remaining then []
  else
    let v0 = Old_Vertex.Set.min_elt remaining in
    let root = walk_up v0 parents ~seen:remaining in
    let tree, seen, remaining =
      traverse_vertex root children ~seen ~remaining
    in
    tree :: wreath_traverse remaining parents children ~seen

let decompose (graph : Old_Graph.t) : t * Old_Edge.Set.t Old_Vertex.Map.t =
  let all_edges = Old_Graph.edges graph in
  let live_edges = Old_Graph.live_edges graph in
  let parents, children = adjacency_maps live_edges in
  let in_degrees = in_degree_map live_edges in
  let multiv, univ, av = partition_vertexes all_edges in_degrees in
  let av = Old_Vertex.Set.remove Old_Vertex.root av in
  let multiparented, seen, remaining =
    traverse_vertexes multiv children ~seen:Old_Vertex.Set.empty ~remaining:univ
  in
  let deleted, seen, remaining =
    traverse_vertexes av children ~seen ~remaining
  in
  let reachable, seen, remaining =
    traverse_vertex Old_Vertex.root children ~seen ~remaining
  in
  let wreaths = wreath_traverse remaining parents children ~seen in
  ({ multiparented; deleted; reachable; wreaths }, children)

(* Unit Tests *)

let%test_module "Old_Graph.decompose" =
  (module struct
    let print_results = true

    let report_tree (prefix : string) (tree : Tree.t) : unit =
      print_string ("\n" ^ prefix ^ ": " ^ Tree.to_string tree)

    let report_trees (prefix : string) (trees : Tree.t list) : unit =
      print_string ("\n" ^ prefix ^ ":");
      List.iter (fun tree -> print_string ("\n  " ^ Tree.to_string tree)) trees

    let check_decompose ?(multiparented : Tree.t list = [])
        ?(deleted : Tree.t list = [])
        ?(reachable : Tree.t = Vertex (Old_Vertex.root, Position_map.empty))
        ?(wreaths : Tree.t list = []) (graph : Old_Graph.t) : bool =
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

    let v0 = Old_Vertex.root

    let v1 = Old_Vertex.mk Exp_plus

    let v2 = Old_Vertex.mk Exp_times

    let e01 = Old_Edge.mk Cursor.{ vertex = v0; position = Root_root_root } v1

    let e12 = Old_Edge.mk Cursor.{ vertex = v1; position = Exp_plus_left } v2

    let e12' = Old_Edge.mk Cursor.{ vertex = v1; position = Exp_plus_right } v2

    let e21 = Old_Edge.mk Cursor.{ vertex = v2; position = Exp_times_left } v1

    let%test "empty graph" = check_decompose Old_Graph.empty

    let%test_module "one vertex" =
      (module struct
        (* well-sorted multiparented and wreaths are impossible *)

        let%test "deleted" =
          check_decompose
            Old_Graph.(empty |> add e01 Deleted)
            ~deleted:[ Tree.vertex v1 [] ]

        let%test "reachable" =
          check_decompose
            Old_Graph.(empty |> add e01 Created)
            ~reachable:
              (Tree.vertex v0
                 [ (Root_root_root, [ (e01.id, Tree.vertex v1 []) ]) ])
      end)

    let%test_module "two vertexes" =
      (module struct
        let%test "multiparented" =
          check_decompose
            Old_Graph.(
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
            Old_Graph.(empty |> add e01 Deleted |> add e12 Created)
            ~deleted:
              [
                Tree.vertex v1
                  [ (Exp_plus_left, [ (e12.id, Tree.vertex v2 []) ]) ];
              ]

        let%test "reachable e01 deleted e12" =
          check_decompose
            Old_Graph.(empty |> add e01 Created |> add e12 Deleted)
            ~deleted:[ Tree.vertex v2 [] ]
            ~reachable:
              (Tree.vertex v0
                 [ (Root_root_root, [ (e01.id, Tree.vertex v1 []) ]) ])

        let%test "reachable e01 e12" =
          check_decompose
            Old_Graph.(empty |> add e01 Created |> add e12 Created)
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
            Old_Graph.(
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
