module Physics = Body.Physics
module C = Centroid
module Int126 = Morton126.Int126

module MortonMap : sig
  type 'a t

  val t_of_sexp : (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a t
  val sexp_of_t : ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t
end

type region =
  { centroid : C.t
  ; bodies : int
  ; size : float
  }

type t =
  { bodies : C.t list MortonMap.t
  ; regions : region MortonMap.t
  ; bounds : Bb.t
  ; theta : float
  }

val region_size : Bb.t -> level:int -> bits_per_dimension:int -> float
val create : Bb.t -> float -> t
val insert_body : C.t -> t -> t
val insert_bodies : C.t list -> t -> t
val build_aggregates : t -> t
val distance : Physics.vec -> Physics.vec -> float
val calculate_force_on_body : Body.t -> t -> Physics.vec
val calculate_all_forces : t -> Body.t list -> (Body.t * Physics.vec) list
val update_simulation : (Body.t * Physics.vec) list -> float -> unit
val gravity_simulation_example : unit -> t
val simulation_timestep : Body.t list -> Bb.t -> theta:float -> dt:float -> unit
