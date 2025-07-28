open! Core
open! Core_bench
open! Body

(* Simple uniform random float between 0 and 1 *)
let uniform_float () =
  Random.bits () |> Int.to_float |> fun x -> x /. Float.of_int Int.max_value
;;

(* Create a random distribution of bodies in 3D space *)
let _create_random_bodies n bounds =
  let open Physics in
  let open Otree.Bb in
  List.init n ~f:(fun _i ->
    let x_range = bounds.x_max -. bounds.x_min in
    let y_range = bounds.y_max -. bounds.y_min in
    let z_range = bounds.z_max -. bounds.z_min in
    let x = (uniform_float () *. x_range) +. bounds.x_min in
    let y = (uniform_float () *. y_range) +. bounds.y_min in
    let z = (uniform_float () *. z_range) +. bounds.z_min in
    let vx = (uniform_float () *. 20.0) -. 10.0 in
    let vy = (uniform_float () *. 20.0) -. 10.0 in
    let vz = (uniform_float () *. 20.0) -. 10.0 in
    let mass = (uniform_float () *. 9000.0) +. 1000.0 in
    Body.{ pos = point x y z; vel = vec vx vy vz; mass })
;;

(* Create bodies in a galaxy-like spiral distribution *)
let create_galaxy_bodies n _bounds =
  let open Physics in
  List.init n ~f:(fun i ->
    let angle = Float.(2. * pi * of_int i / of_int n * 3.) in
    (* 3 spiral arms *)
    let radius = sqrt (uniform_float ()) *. 80.0 in
    (* sqrt for even distribution *)
    let height = (uniform_float () *. 10.0) -. 5.0 in
    let x = radius *. Float.cos angle in
    let y = radius *. Float.sin angle in
    let z = height in
    (* Orbital velocity around center *)
    let orbital_speed = sqrt (10000. /. (radius +. 1.)) in
    let vx = orbital_speed *. Float.sin angle *. -1. in
    let vy = orbital_speed *. Float.cos angle in
    let vz = (uniform_float () *. 2.0) -. 1.0 in
    let mass = (uniform_float () *. 900.0) +. 100.0 in
    Body.{ pos = point x y z; vel = vec vx vy vz; mass })
;;

(* Benchmark single timestep *)
let bench_single_timestep n_bodies theta dt =
  let bounds =
    Otree.Bb.
      { x_min = -100.
      ; x_max = 100.
      ; y_min = -100.
      ; y_max = 100.
      ; z_min = -100.
      ; z_max = 100.
      }
  in
  let bodies = create_galaxy_bodies n_bodies bounds in
  fun () -> Otree.simulation_timestep bodies bounds ~theta ~dt
;;

(* Benchmark multiple timesteps *)
let bench_multiple_timesteps n_bodies n_steps theta dt =
  let bounds =
    Otree.Bb.
      { x_min = -100.
      ; x_max = 100.
      ; y_min = -100.
      ; y_max = 100.
      ; z_min = -100.
      ; z_max = 100.
      }
  in
  let bodies = create_galaxy_bodies n_bodies bounds in
  fun () ->
    for _ = 1 to n_steps do
      Otree.simulation_timestep bodies bounds ~theta ~dt
    done
;;


let benchmarks =
  [ (* Reduced benchmark set for faster testing *)
    Bench.Test.create
      ~name:"1K bodies - single timestep"
      (bench_single_timestep 1000 0.5 0.01)
  ; Bench.Test.create
      ~name:"5K bodies - single timestep"
      (bench_single_timestep 5000 0.5 0.01)
  ; Bench.Test.create
      ~name:"10K bodies - single timestep"
      (bench_single_timestep 10000 0.5 0.01)
  ; (* Test different theta values with smaller body count *)
    Bench.Test.create
      ~name:"5K bodies - high precision (θ=0.1)"
      (bench_single_timestep 5000 0.1 0.01)
  ; (* Multiple timesteps with reduced count *)
    Bench.Test.create
      ~name:"5K bodies - 5 timesteps"
      (bench_multiple_timesteps 5000 5 0.5 0.01)
  ]
;;

let () =
  Random.init 42;
  (* Seed for reproducible benchmarks *)
  Command_unix.run (Bench.make_command benchmarks)
;;
