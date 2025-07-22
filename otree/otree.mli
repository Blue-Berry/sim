module Physics = Body.Physics
open Utils
module Bb = Bb

module C : sig
  type t =
    { p : Physics.point
    ; m : float
    }

  val pp : Format.formatter -> t -> unit
  val empty : t
  val add : t -> t -> t
end

type tree =
  { mutable size : int
  ; capacity : int
  ; mass_xyz : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t
  ; mass : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
  ; children : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array2.t
  }

val print_tree : tree -> unit
val create_tree : capacity:int -> tree

val has_children
  :  (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array2.t
  -> int
  -> bool

val update_centroid : tree -> int -> C.t -> unit
val get_centroid : tree -> int -> C.t
val grow_tree : tree -> tree
val append_leaf : tree -> C.t -> int
val insert_body : tree -> C.t -> Bb.t -> unit
