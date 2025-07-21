module Physics = Body.Physics
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

module LinearOctree : sig
  module Int126 = Morton126.Int126

  module MortonMap : sig
    type 'a t
  end

  type 'a entry =
    { point : Physics.point
    ; value : 'a
    }

  type 'a t =
    { data : 'a entry list MortonMap.t
    ; bounds : Bb.t
    }

  val create : Bb.t -> 'a t
  val insert : Morton126.Physics.point -> 'a -> 'a t -> 'a t
  val range_query : Bb.t -> 'a t -> 'a entry list

  val nearest_neighbor
    :  (float, 'a) Owl_dense_matrix_generic.t
    -> int
    -> 'b t
    -> 'b entry list

  val bulk_insert : (Morton126.Physics.point * 'a) list -> 'a t -> 'a t
  val to_list : 'a t -> 'a entry list
  val size : 'a t -> int
end

val example_usage
  :  unit
  -> string LinearOctree.t
     * string LinearOctree.entry list
     * string LinearOctree.entry list
