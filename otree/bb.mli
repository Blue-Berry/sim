module Physics = Body.Physics

type t =
  { x_min : float
  ; x_max : float
  ; y_min : float
  ; y_max : float
  ; z_min : float
  ; z_max : float
  }

val sexp_of_t : t -> Sexplib0.Sexp.t

type octant =
  | O1
  | O2
  | O3
  | O4
  | O5
  | O6
  | O7
  | O8

val sexp_of_octant : octant -> Sexplib0.Sexp.t
val octant_of_point : Body.Physics.point -> t -> octant
val octant_bb : t -> octant -> t
val parent_bb : t -> octant -> t
val int_of_octant : octant -> int
