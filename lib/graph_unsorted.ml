type ctor =
  | Root_root
  | Id_id of string
  | Exp_lam
  | Exp_app
  | Exp_var
  | Typ_app
  | Typ_var

type index =
  | Root_root_root
  | Exp_lam_param
  | Exp_lam_param_type
  | Exp_lam_body
  | Exp_app_fun
  | Exp_app_arg
  | Exp_var_id
  | Typ_app_fun

type vertex = Vertex of Uuid.t * ctor

type edge = { id : Uuid.t; source : vertex; index : index; target : vertex }

type edge_state = Created | Destroyed

(* Note: edges not in the table have not been created yet and are `\bot` *)
let edges_table : (edge, edge_state) Hashtbl.t = Hashtbl.create 0

(* TODO: use `Set.t` instead of `list`?*)
let edges_from_table : (vertex * index, edge list) Hashtbl.t = Hashtbl.create 0

let edges_to_table : (vertex, edge list) Hashtbl.t = Hashtbl.create 0

let update_edge : edge -> edge_state -> unit =
 fun edge edge_state ->
  let old_state = Hashtbl.find_opt edges_table edge in
  let action : edge_state option =
    match old_state with
    | Some Destroyed -> None
    | Some Created -> (
        match edge_state with Destroyed -> Some Destroyed | Created -> None )
    | None -> Some edge_state
  in
  match action with
  | None -> ()
  | Some Created ->
      Hashtbl.replace edges_table edge edge_state;
      let new_edges_from =
        Option.value
          (Hashtbl.find_opt edges_from_table (edge.source, edge.index))
          ~default:[]
      in
      Hashtbl.replace edges_from_table (edge.source, edge.index)
        (edge :: new_edges_from);
      let new_edges_to =
        Option.value (Hashtbl.find_opt edges_to_table edge.target) ~default:[]
      in
      Hashtbl.replace edges_to_table edge.target (edge :: new_edges_to)
      (* TODO: short circuit if deleting a non-existant *)
  | Some Destroyed -> (
      Hashtbl.replace edges_table edge Destroyed;
      match old_state with
      | None -> ()
      | _ ->
          let old_edges_from =
            Option.value
              (Hashtbl.find_opt edges_from_table (edge.source, edge.index))
              ~default:[]
          in
          let new_edges_from = List.filter (( == ) edge) old_edges_from in
          Hashtbl.replace edges_from_table (edge.target, edge.index)
            new_edges_from;
          let old_edges_to =
            Option.value
              (Hashtbl.find_opt edges_to_table edge.target)
              ~default:[]
          in
          let new_edges_to = List.filter (( == ) edge) old_edges_to in
          Hashtbl.replace edges_to_table edge.target new_edges_to )

let edges_from : vertex -> index -> edge list =
 fun vertex index ->
  match Hashtbl.find_opt edges_from_table (vertex, index) with
  | None -> []
  | Some edges -> edges

let edges_to : vertex -> edge list =
 fun vertex ->
  match Hashtbl.find_opt edges_to_table vertex with
  | None -> []
  | Some edges -> edges
