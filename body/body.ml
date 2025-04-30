open! Core
open! Owl.Maths

module Physics = struct
  type point =
    { x : float
    ; y : float
    ; z : float
    }
  [@@deriving sexp_of]

  type vec =
    { x : float
    ; y : float
    ; z : float
    }
  [@@deriving sexp_of]

  let displace ({ x; y; z } : point) ({ x = dx; y = dy; z = dz } : vec) : point =
    { x = x +. dx; y = y +. dy; z = z +. dz }
  ;;

  let mag_sq ({ x; y; z } : vec) : float = sqr x +. sqr y +. sqr z

  (*   TODO: Fast inverse square root *)
  let mag v = sqrt (mag_sq v)
  let ( *$ ) c ({ x; y; z } : vec) = { x = x *. c; y = y *. c; z = z *. c }
  let unit_vec (v : vec) : vec = 1.0 /. mag v *$ v [@@inline]

  let ( --> ) ({ x = x1; y = y1; z = z1 } : point) ({ x = x2; y = y2; z = z2 } : point)
    : vec
    =
    { x = x2 -. x1; y = y2 -. y1; z = z2 -. z1 }
  ;;

  let ( +$ ) ({ x = x1; y = y1; z = z1 } : vec) ({ x = x2; y = y2; z = z2 } : vec) : vec =
    { x = x1 +. x2; y = y1 +. y2; z = z1 +. z2 }
  ;;

  let zero : vec = { x = 0.0; y = 0.0; z = 0.0 }

  let close_enough
        ({ x = x1; y = y1; z = z1 } : point)
        ({ x = x2; y = y2; z = z2 } : point)
    =
    Float.(mag_sq { x = x1 -. x2; y = y1 -. y2; z = z1 -. z2 } <. 1.0e-5)
  ;;

  let g = 6.67428e-11 (* N (m/kg)^2 *)

  (** [acc_on] [p] [c] Acceleration on body at point [p] due to body [c] *)
  let acc_on (p1 : point) (p2 : point) m2 : vec =
    match close_enough p1 p2 with
    | true -> zero
    | false ->
      let d = p1 --> p2 in
      let r = mag_sq d in
      let aMag = g *. m2 /. r in
      let aDir = unit_vec d in
      aMag *$ aDir
  ;;

  (** [new_position] [pos] [t] [v] [a] New position of body [pos] after time [t] with velocity [v] and acceleration [a] *)
  let new_position (pos : point) (t : float) (v : vec) (a : vec) : point =
    (*   TODO: replace with runga kutta *)
    displace pos ((t *$ v) +$ (0.5 *. t *. t *$ a))
  ;;

  let new_velocity (v : vec) (a : vec) (t : float) : vec = v +$ (t *$ a)
end

open Physics

type t =
  { mass : float
  ; mutable pos : point
  ; mutable vel : vec
  }
[@@deriving sexp_of]

let step b a t =
  let pos' = new_position b.pos t b.vel a in
  let vel' = new_velocity b.vel a t in
  b.pos <- pos';
  b.vel <- vel'
;;

let accelerate_body (bodies : t list) (pos1 : point) : vec =
  let combine_accelerations (acc : vec) (b : t) : vec = acc_on pos1 b.pos b.mass +$ acc in
  List.fold_left ~init:zero ~f:combine_accelerations bodies
;;

let accelerations (bodies : t list) : vec list =
  let calc_acc (b : t) : vec =
    let bodies = List.filter ~f:(fun b' -> not @@ phys_equal b.pos b'.pos) bodies in
    accelerate_body bodies b.pos
  in
  List.map ~f:calc_acc bodies
;;

let step_bodies (bodies : t list) (dt : float) : unit =
  let accels = accelerations bodies in
  match List.iter2 accels bodies ~f:(fun a b -> step b a dt) with
  | List.Or_unequal_lengths.Ok () -> ()
  | List.Or_unequal_lengths.Unequal_lengths -> failwith "Unequal lengths"
;;
