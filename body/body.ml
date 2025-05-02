open! Core
open! Owl

module Physics = struct
  open! Owl.Maths
  open Owl.Mat

  type point = Owl.Mat.mat
  type vec = Owl.Mat.mat

  let pp ppf v =
    Format.fprintf ppf "x: %.4f y: %.4f z: %.4f" v.%{0, 0} v.%{0, 1} v.%{0, 2}
  ;;

  let print v = Format.fprintf Format.std_formatter "%a\n" pp v
  let displace (p : point) (v : vec) : point = p + v

  (* let mag_sq v : float = v |> sqr |> sum_cols |> trace *)
  let mag_sq : vec -> float = fun v -> v |> sqr |> sum_cols |> trace

  (*   TODO: Fast inverse square root *)
  let mag v = Maths.sqrt (mag_sq v)
  let unit_vec (v : vec) : vec = 1.0 /. mag v $* v [@@inline]
  let ( --> ) (v1 : point) (v2 : point) : vec = v2 - v1
  let zero : vec = zeros 1 3

  let close_enough (p1 : point) (p2 : point) =
    let d = p2 - p1 in
    Float.(mag_sq d <. 1.0e-5)
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
      aMag $* aDir
  ;;

  (** [new_position] [pos] [t] [v] [a] New position of body [pos] after time [t] with velocity [v] and acceleration [a] *)
  let new_position (pos : point) (t : float) (v : vec) (a : vec) : point =
    (*   TODO: replace with runga kutta *)
    displace pos ((t $* v) + (0.5 *. t *. t $* a))
  ;;

  let new_velocity (v : vec) (a : vec) (t : float) : vec = v + (t $* a)
end

open Physics

type t =
  { mass : float
  ; mutable pos : point
  ; mutable vel : vec
  }

let pp ppf b =
  let open Format in
  fprintf ppf "M: %.2f\nP: %a\nV: %a\n" b.mass Physics.pp b.pos Physics.pp b.vel
;;

let print b = pp Format.std_formatter b

let step b a t =
  let pos' = new_position b.pos t b.vel a in
  let vel' = new_velocity b.vel a t in
  b.pos <- pos';
  b.vel <- vel'
;;

let accelerate_body (bodies : t list) pos1 : vec =
  let open Mat in
  let combine_accelerations (acc : vec) (b : t) : vec = acc_on pos1 b.pos b.mass + acc in
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

module Utils = struct
  let orbital_velocity ~m1 ~m2 ~r = sqrt (m2 *. m2 *. g /. ((m1 +. m2) *. r))

  let gen_dist (d : float) : float =
    let iter = 10 in
    let k = d /. float_of_int iter in
    let rec loop i acc = if i = 0 then acc else loop (i - 1) (Random.float k +. acc) in
    loop iter 0.0 -. (d /. 2.)
  ;;

  (* calculate the tangent vector to a circle centered at the origin
   at the point p *)
  (* let tangent_vector ({ x; y; z } : Physics.point) : vec = *)
  (*   let theta = atan2 x y in *)
  (*   let vx = 0. -. cos theta in *)
  (*   let vy = sin theta in *)
  (* ;; *)
end

let orthogonal_vector (v : Owl.Mat.mat) : Owl.Mat.mat =
  let open Owl.Mat in
  (*     get the nullspace of v *)
  let nullspace = Owl.Linalg.D.null v |> to_cols in
  assert (Int.equal (Array.length nullspace) 2);
  let u1 = nullspace.(0) in
  let u2 = nullspace.(1) in
  let a = Random.float 10. in
  let b = Random.float 10. in
  (* TODO: normalize *)
  (a $* u1) + (b $* u2)
;;
