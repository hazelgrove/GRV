(** A flag to prevent concurrent GUI input actions

    Set to [true] when an action is being handled. *)

type t = bool ref
