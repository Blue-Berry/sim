module Physics = Body.Physics

module Int126 : sig
  type t =
    { high : int
    ; low : int
    }

  val t_of_sexp : Base.Sexp.t -> t
  val sexp_of_t : t -> Base.Sexp.t
  val zero : t
  val one : t
  val max_63_bit : int
  val mask_63 : int
  val of_int : int -> t
  val shift_left : t -> int -> t
  val shift_right_logical : t -> int -> t
  val logand : t -> t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val test_bit : t -> int -> bool
  val to_hex : t -> string
  val to_string : t -> string
end

type t = Int126.t

val precision : int
val encode : int -> int -> int -> Int126.t
val decode : Int126.t -> int * int * int
val point_to_grid : Physics.point -> Bb.t -> int * int * int
val encode_point : Physics.point -> Bb.t -> Int126.t
