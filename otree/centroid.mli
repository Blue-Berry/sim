type t =
  { p : Body.Physics.point
  ; m : float
  }

val pp : Format.formatter -> t -> unit
val empty : t
val sexp_of_t : t -> Sexplib0.Sexp.t
val add : t -> t -> t
