type t = {
  id : Uuid.Id.t;
  graph : Old_Graph.t;
  cursor : Cursor.t;
  actions : Graph_action.Set.t;
  known_actions : Graph_action.Set.t;
  show_ids : bool;
}

let sexp_of_t (editor : t) : Sexplib.Sexp.t =
  Sexplib.Sexp.List
    [
      Uuid.Id.sexp_of_t editor.id;
      Old_Graph.sexp_of_t editor.graph;
      Cursor.sexp_of_t editor.cursor;
      Sexplib.Std.sexp_of_list Graph_action.sexp_of_t
        (Graph_action.Set.elements editor.actions);
      Sexplib.Std.sexp_of_list Graph_action.sexp_of_t
        (Graph_action.Set.elements editor.known_actions);
      Sexplib.Std.sexp_of_bool editor.show_ids;
    ]

let t_of_sexp : Sexplib.Sexp.t -> t = function
  | Sexplib.Sexp.List
      [
        id_sexp;
        graph_sexp;
        cursor_sexp;
        actions_sexp;
        known_actions_sexp;
        show_ids_sexp;
      ] ->
      let id = Uuid.Id.t_of_sexp id_sexp in
      let graph = Old_Graph.t_of_sexp graph_sexp in
      let cursor = Cursor.t_of_sexp cursor_sexp in
      let actions =
        Sexplib.Std.list_of_sexp Graph_action.t_of_sexp actions_sexp
        |> Graph_action.Set.of_list
      in
      let known_actions =
        Sexplib.Std.list_of_sexp Graph_action.t_of_sexp known_actions_sexp
        |> Graph_action.Set.of_list
      in
      let show_ids = Sexplib.Std.bool_of_sexp show_ids_sexp in
      { id; graph; cursor; actions; known_actions; show_ids }
  | _ -> failwith __LOC__

let sexp_of_map (editors : t Uuid.Map.t) : Sexplib.Sexp.t =
  Uuid.Map.bindings editors
  |> Sexplib.Std.sexp_of_list (fun (id, editor) ->
         Sexplib.Sexp.List [ Uuid.Id.sexp_of_t id; sexp_of_t editor ])

let map_of_sexp (sexp : Sexplib.Sexp.t) : t Uuid.Map.t =
  sexp
  |> Sexplib.Std.list_of_sexp (function
       | Sexplib.Sexp.List [ id_sexp; editor_sexp ] ->
           (Uuid.Id.t_of_sexp id_sexp, t_of_sexp editor_sexp)
       | _ -> failwith __LOC__)
  |> List.to_seq |> Uuid.Map.of_seq

let mk () : t =
  {
    id = Uuid.Id.next ();
    graph = Old_Graph.empty;
    cursor = Cursor.root;
    actions = Graph_action.Set.empty;
    known_actions = Graph_action.Set.empty;
    show_ids = false;
  }
