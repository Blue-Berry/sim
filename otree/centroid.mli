type t =
  { p : Body.Physics.point
  ; m : float
  }

val pp : Format.formatter -> t -> unit
val empty : t
val add : t -> t -> t
