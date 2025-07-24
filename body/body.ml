open! Core
open! Owl

module Physics = struct
  open! Owl.Maths
  open Owl.Mat

  type point = Owl.Mat.mat
  type vec = Owl.Mat.mat

  let ( .%{} ) x dim =
    match dim with
    | `x -> Mat.( .%{} ) x (0, 0)
    | `y -> Mat.( .%{} ) x (0, 1)
    | `z -> Mat.( .%{} ) x (0, 2)
  ;;

  let point (a : float) (b : float) (c : float) : point =
    let v1 = vector 3 in
    v1.%{0, 0} <- a;
    v1.%{0, 1} <- b;
    v1.%{0, 2} <- c;
    v1
  ;;

  let vec : float -> float -> float -> vec = point
  let pp ppf v = Format.fprintf ppf "x: %.4e y: %.4e z: %.4e" v.%{`x} v.%{`y} v.%{`z}
  let print v = Format.fprintf Format.std_formatter "%a\n" pp v
  let displace (p : point) (v : vec) = add_ p v

  let sexp_of_point (p : point) =
    Sexp.(
      List
        [ List [ Atom "x"; Float.sexp_of_t p.%{`x} ]
        ; List [ Atom "y"; Float.sexp_of_t p.%{`y} ]
        ; List [ Atom "z"; Float.sexp_of_t p.%{`z} ]
        ])
  ;;

  (* let mag_sq v : float = v |> sqr |> sum_cols |> trace *)
  let mag_sq : vec -> float = fun v -> v |> sqr |> sum_cols |> trace

  (*   TODO: Fast inverse square root *)
  let mag v = Maths.sqrt (mag_sq v)

  (* let unit_vec (v : vec) : vec = 1.0 /. mag v $* v *)
  let vec_norm (v : vec) : vec =
    let norm = Linalg.D.norm v in
    v /$ norm
  ;;

  (* [v1 --> v2] Vec from [v1] to [v2] *)
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
      let aDir = vec_norm d in
      aMag $* aDir
  ;;

  (** [new_position] [pos] [t] [v] [a] New position of body [pos] after time [t] with velocity [v] and acceleration [a] *)
  let new_position ~(pos : point) ~(t : float) ~(v : vec) ~(a : vec) =
    (*   TODO: replace with runga kutta *)
    displace pos ((t $* v) + (0.5 *. t *. t $* a))
  ;;

  let new_velocity ~(v : vec) ~(a : vec) ~(t : float) : unit = add_ v (t $* a)
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
  new_position ~pos:b.pos ~t ~v:b.vel ~a;
  new_velocity ~v:b.vel ~a ~t
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
  let v a b c =
    let open Owl.Mat in
    let v1 = vector 3 in
    v1.%{0, 0} <- a;
    v1.%{0, 1} <- b;
    v1.%{0, 2} <- c;
    v1
  ;;

  let dist = 4000000000.0
  let orbital_velocity ~m1 ~m2 ~r = sqrt (m2 *. m2 *. g /. ((m1 +. m2) *. r))

  let gen_dist (d : float) : float =
    let iter = 10 in
    let k = d /. float_of_int iter in
    let rec loop i acc = if i = 0 then acc else loop (i - 1) (Random.float k +. acc) in
    loop iter 0.0 -. (d /. 2.)
  ;;

  let orthogonal_vector (v : Owl.Mat.mat) : Owl.Mat.mat =
    let open Owl.Mat in
    (*     get the nullspace of v *)
    let nullspace = Owl.Linalg.D.null v |> to_cols in
    assert (Int.equal (Array.length nullspace) 2);
    let u1 = nullspace.(0) |> transpose in
    let u2 = nullspace.(1) |> transpose in
    let a = Random.float 10. in
    let b = Random.float 10. in
    (a $* u1) + (b $* u2) |> Physics.vec_norm
  ;;

  (* Make a list of n bodies with random masses, distributed around
   a central star. Initialize their starting velocities to put
   them into circular orbits. *)
  let mk_lots (star_mass : float) (n : int) : t list =
    let _ = Random.init 17 in
    let star m = { mass = m; pos = v 0. 0. 0.; vel = v 0. 0. 0. } in
    let mk_one () : t =
      let d = dist *. 2. in
      let mass = Random.float 1000.0 *. 1e13 in
      let x = gen_dist d in
      let y = gen_dist d in
      let z = gen_dist d in
      let p = v x y z in
      let r = Physics.mag p in
      let v = orthogonal_vector p in
      let velocity = orbital_velocity ~m1:mass ~m2:star_mass ~r in
      { mass; pos = p; vel = Owl.Mat.( $* ) velocity v }
    in
    let rec loop n acc = if n = 0 then acc else loop (n - 1) (mk_one () :: acc) in
    loop n [ star star_mass ]
  ;;

  (* Displace all of the bodies in a list by a given vector *)
  let displace_bodies (bodies : t list) (v : vec) : unit =
    List.iter ~f:(fun b -> displace b.pos v) bodies
  ;;

  (* Add a fixed velocity vector to each of the bodies' velocities.
   (This sets an entire collection of bodies moving at a constant
   velocity.) *)
  let add_velocity (bodies : t list) (v : vec) : unit =
    let open Owl.Mat in
    List.iter ~f:(fun b -> b.vel <- b.vel + v) bodies
  ;;

  let collision : t list =
    let sun_mass = 2.0e30 in
    let star_mass = sun_mass *. 0.1 in
    let solar1 = mk_lots sun_mass 10000 in
    let solar2 = mk_lots star_mass 3000 in
    let disp1 = v (dist /. 4.) 0.0 0. in
    let disp2 = v (0.0 -. (dist /. 4.)) (0.0 -. (dist /. 4.)) 0. in
    let diff = disp1 --> disp2 in
    let v2 =
      Owl.Mat.( $* )
        (orbital_velocity ~m1:star_mass ~m2:sun_mass ~r:(mag diff) *. 0.9)
        (orthogonal_vector diff)
    in
    displace_bodies solar1 disp1;
    displace_bodies solar2 disp2;
    add_velocity solar2 v2;
    solar2 @ solar1
  ;;

  let sun_mass = 2.0e30
  let star m = { mass = m; pos = v 0. 0. 0.; vel = v 0. 0. 0. }

  let mercury () : t =
    { mass = 3.30e23; pos = v 57910000.0 0. 0.; vel = v 9000.0 9000.0 0. }
  ;;

  let venus () : t =
    { mass = 4.87e24; pos = v 0.0 108200000.0 0.; vel = v (-9000.0) 0.0 0. }
  ;;

  let earth () : t =
    { mass = 5.98e24; pos = v (-149600000.0) 0.0 0.; vel = v 0.0 (-9000.0) 0. }
  ;;

  let mars () : t =
    { mass = 6.42e23; pos = v 0.0 (-227940000.0) 0.; vel = v 600000.0 0.0 0. }
  ;;

  let jupiter () : t =
    { mass = 1.90e27; pos = v 778330000.0 0.0 0.; vel = v 0.0 450000.0 0. }
  ;;

  let saturn () : t =
    { mass = 5.69e26; pos = v 0.0 1426940000.0 0.; vel = v (-300000.0) 0.0 0. }
  ;;

  let uranus () : t =
    { mass = 8.69e25; pos = v (-2870990000.0) 0.0 0.; vel = v 0.0 (-200000.0) 0. }
  ;;

  let planets =
    [ star sun_mass
    ; mars ()
    ; jupiter ()
    ; saturn ()
    ; uranus ()
    ; mercury ()
    ; venus ()
    ; earth ()
    ]
  ;;
end
