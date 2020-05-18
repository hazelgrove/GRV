module Js = Js_of_ocaml.Js

let of_graph (graph : Graph.t) (cursor : Cursor.t) : string =
  let nodes =
    List.map
      (fun (key, vertex) ->
        let open Lang.Constructor in
        let open Printf in
        let id = Uuid.Id.show key in
        let shape =
          if vertex = Vertex.root then "point"
          else
            Option.value
              (Option.map
                 (fun v -> if vertex = v then "doublecircle" else "circle")
                 (Cache.vertex cursor graph.cache))
              ~default:"circle"
        in
        let label =
          match Uuid.Wrap.unmk vertex with
          | Root_root -> {|"Root"|}
          | Id_id str -> sprintf {|"Id\n%s"|} str
          | Exp_var -> {|"Var"|}
          | Exp_lam -> {|"Lam"|}
          | Exp_app -> sprintf {|"App\n%s"|} id
          | Exp_num _num -> {|"Num"|}
          | Exp_plus -> {|"Plus"|}
          | Typ_num -> {|"Num Type"|}
          | Typ_arrow -> {|"Arrow"|}
        in
        Printf.sprintf "n%s [shape=%s,label=%s]" id shape label)
      (List.of_seq @@ Uuid.Map.to_seq graph.cache.vertexes)
  in
  let edges =
    List.map
      (fun edge ->
        let label =
          match (Edge.source edge).index with
          | Root_root_root -> {|"root"|}
          | Exp_var_id -> {|"var"|}
          | Exp_lam_param -> {|"param"|}
          | Exp_lam_param_type -> {|"type"|}
          | Exp_lam_body -> {|"body"|}
          | Exp_app_fun -> {|"fun"|}
          | Exp_app_arg -> {|"arg"|}
          | Exp_plus_left -> {|"left"|}
          | Exp_plus_right -> {|"right"|}
          | Typ_arrow_arg -> {|"arg"|}
          | Typ_arrow_result -> {|"result"|}
        in
        Printf.sprintf "n%s -- n%s [label=%s]"
          (Uuid.Id.show (Edge.source edge).vertex.id)
          (Uuid.Id.show (Edge.target edge).id)
          label)
      List.(map snd @@ of_seq @@ Uuid.Map.to_seq graph.cache.edges)
  in
  "graph G {{rank=min; n0 [shape=point]};" ^ {|node [margin="0,0"];|}
  ^ String.concat ";" nodes ^ ";" ^ String.concat ";" edges ^ "}"

let draw (instance : int) (graph : Graph.t) (cursor : Cursor.t) : unit =
  Js.Unsafe.js_expr
  @@ Printf.sprintf "drawViz('graph%d', '%s')" instance
       (String.escaped @@ of_graph graph cursor)
