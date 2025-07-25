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
  ; mutable capacity : int
  ; mutable mass_xyz : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t
  ; mutable mass : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
  ; mutable children : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array2.t
  ; theta : float
  }

val print_tree : tree -> unit
val create_tree : capacity:int -> theta:float -> tree

val has_children
  :  (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array2.t
  -> int
  -> bool

val update_centroid : tree -> int -> C.t -> unit
val get_centroid : tree -> int -> C.t
val grow_tree : tree -> unit
val append_leaf : tree -> C.t -> int
val insert_body : tree -> C.t -> Bb.t -> unit
val distance : Body.Physics.vec -> Body.Physics.vec -> float
val bb_size : Bb.t -> float
val calculate_force_on_body : Body.t -> tree -> Bb.t -> Body.Physics.vec
val calculate_all_forces : tree -> Body.t list -> Bb.t -> (Body.t * Body.Physics.vec) list
val update_simulation : (Body.t * Body.Physics.vec) list -> float -> unit
val simulation_timestep : Body.t list -> Bb.t -> theta:float -> dt:float -> unit
