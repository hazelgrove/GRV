module Make (T : sig
  type t

  val compare : t -> t -> int
end) =
struct
  include Set.Make (struct
    type t = T.t

    let compare = T.compare
  end)
end
