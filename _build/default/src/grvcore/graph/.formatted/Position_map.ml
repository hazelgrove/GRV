module OrderedType = struct
  type t = Lang.Position.t

  let compare = Lang.Position.compare
end

include Map.Make (OrderedType)

let get ?(default : 'a list = []) (position : key) (map : 'a list t) : 'a list =
  find_opt position map |> Option.value ~default

let push (position : key) (x : 'a) (map : 'a list t) : 'a list t =
  let xs = get position map in
  remove position map |> add position (x :: xs)
