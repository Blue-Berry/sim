module Physics = Body.Physics

module Int128 : sig
  type t =
    { high : Unsigned.uint64
    ; low : Unsigned.uint64
    }

  val zero : t
  val one : t
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
  val of_hex : 'a -> 'b
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val t_of_sexp : Sexplib0.Sexp.t -> 'a
  val popcount : t -> int
end

type t = Int128.t

val bits_per_dimension : int
val encode : int -> int -> int -> Int128.t
val decode : Int128.t -> int * int * int
val point_to_grid : Physics.point -> Bb.t -> int * int * int

(* Float coordinate conversion *)
val encode_point : Physics.point -> Bb.t -> Int128.t

(* Get parent Morton code by removing 3 least significant bits *)
val parent_morton : Int128.t -> Int128.t

(* Get Morton code at specific level (0 = leaf, higher = more aggregated) *)
val morton_at_level : Int128.t -> int -> Int128.t
