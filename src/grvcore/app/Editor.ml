type t = {
  id : Uuid.Id.t;
  graph : Graph.t;
  cursor : Cursor.t;
  actions : Graph_action.Set.t;
  known_actions : Graph_action.Set.t;
}

let sexp_of_t (editor : t) : Sexplib.Sexp.t =
  Sexplib.Sexp.List
    [
      Uuid.Id.sexp_of_t editor.id;
      Graph.sexp_of_t editor.graph;
      Cursor.sexp_of_t editor.cursor;
      Sexplib.Std.sexp_of_list Graph_action.sexp_of_t
        (Graph_action.Set.elements editor.actions);
      Sexplib.Std.sexp_of_list Graph_action.sexp_of_t
        (Graph_action.Set.elements editor.known_actions);
    ]

let t_of_sexp : Sexplib.Sexp.t -> t = function
  | Sexplib.Sexp.List
      [ id_sexp; graph_sexp; cursor_sexp; actions_sexp; known_actions_sexp ] ->
      let id = Uuid.Id.t_of_sexp id_sexp in
      let graph = Graph.t_of_sexp graph_sexp in
      let cursor = Cursor.t_of_sexp cursor_sexp in
      let actions =
        Graph_action.Set.of_list
          (Sexplib.Std.list_of_sexp Graph_action.t_of_sexp actions_sexp)
      in
      let known_actions =
        Graph_action.Set.of_list
          (Sexplib.Std.list_of_sexp Graph_action.t_of_sexp known_actions_sexp)
      in
      { id; graph; cursor; actions; known_actions }
  | _ -> failwith __LOC__

let mk () : t =
  {
    id = Uuid.Id.next ();
    graph = Graph.empty;
    cursor = Cursor.root;
    actions = Graph_action.Set.empty;
    known_actions = Graph_action.Set.empty;
  }

(* module OrderedType = struct
  type nonrec t = t

  let compare : t -> t -> int = Int.compare
end *)
