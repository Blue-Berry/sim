module Physics = Body.Physics

module Int128 : sig
  type t =
    { high : Unsigned.uint64
    ; low : Unsigned.uint64
    }

  val zero : t
  val one : t
  val of_int : int -> t
  val of_uint64 : Unsigned.uint64 -> t
  val add : t -> t -> t
  val shift_left : t -> int -> t
  val shift_right_logical : t -> int -> t
  val logand : t -> t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val test_bit : t -> int -> bool
  val to_hex : t -> string
  val of_hex : string -> t
  val sexp_of_t : t -> Core.Sexp.t
  val t_of_sexp : Core.Sexp.t -> t
  val popcount : t -> int
end

type t = Int128.t

val bits_per_dimension : int
val encode : Unsigned.uint64 -> Unsigned.uint64 -> Unsigned.uint64 -> Int128.t
val decode : Int128.t -> Unsigned.uint64 * Unsigned.uint64 * Unsigned.uint64
val uint64_of_float : Core.Float.t -> Unsigned.uint64
val float_of_uint64 : Unsigned.uint64 -> float

val point_to_grid
  :  Physics.point
  -> Bb.t
  -> Unsigned.uint64 * Unsigned.uint64 * Unsigned.uint64

val encode_point : Physics.point -> Bb.t -> Int128.t
val parent_morton : Int128.t -> Int128.t
val morton_at_level : Int128.t -> int -> Int128.t
