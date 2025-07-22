module Physics = Body.Physics

module LinearOctree : sig
  module Int126 = Morton126.Int126

  module MortonMap : sig
    type 'a t

    val t_of_sexp : (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a t
    val sexp_of_t : ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t
  end

  type 'a entry =
    { point : Physics.vec
    ; value : 'a
    }

  type 'a t =
    { data : 'a entry list MortonMap.t
    ; bounds : Bb.t
    }

  val create : Bb.t -> 'a t
  val insert : Physics.vec -> 'a -> 'a t -> 'a t
  val range_query : Bb.t -> 'a t -> 'a entry list

  val nearest_neighbor
    :  (float, 'a) Owl_dense_matrix_generic.t
    -> int
    -> 'b t
    -> 'b entry list

  val bulk_insert : (Physics.vec * 'a) list -> 'a t -> 'a t
  val to_list : 'a t -> 'a entry list
  val size : 'a t -> int
end

val example_usage
  :  unit
  -> string LinearOctree.t
     * string LinearOctree.entry list
     * string LinearOctree.entry list
