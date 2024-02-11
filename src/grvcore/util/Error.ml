type error = { origin : string; vertex : Vertex.t; message : string }
type 'a t = Success of 'a | Error of error list

let error (origin : string) (vertex : Vertex.t) :
    ('a, unit, string, 'b t) format4 -> 'a =
  Printf.ksprintf (fun message -> Error [ { origin; vertex; message } ])

(* TODO: let with_message ('a, unit, string, 'b t -> 'b t) format4 -> 'a =
   Printf.ksprintf (fun message x -> match x with Success x -> Success x | Error es -> List.map (fun e -> {e with message = })) *)

module Let_syntax = struct
  let bind (o : 'a t) ~(f : 'a -> 'b t) : 'b t =
    match o with Error es -> Error es | Success a -> f a

  let return (o : 'a) : 'a t = Success o

  let map ~(f : 'a -> 'b) (o : 'a t) : 'b t =
    match o with Error _ -> o | Success a -> return (f a)

  let both (o1 : 'a t) (o2 : 'b t) : ('a * 'b) t =
    match (o1, o2) with
    | Error es1, Error es2 -> Error (es1 @ es2)
    | Error es1, _ -> Error es1
    | _, Error es2 -> Error es2
    | Success o1, Success o2 -> Success (o1, o2)
end

include Let_syntax

let rec sequence (xs : 'a t list) : 'a list t =
  match xs with
  | [] -> return []
  | x :: xs ->
      let%bind x' = x and xs' = sequence xs in
      return (x' :: xs')
