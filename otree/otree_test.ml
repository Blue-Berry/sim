open! Core
open! Otree
module Bb = Otree.Bb
module Physics = Body.Physics

let bb = Bb.{ x_min = -1.; x_max = 1.; y_min = -1.; y_max = 1.; z_min = -1.; z_max = 1. }

let%expect_test "octant_bb" =
  print_s [%sexp (Bb.octant_bb bb Bb.O1 : Bb.t)];
  [%expect {| ((x_min 0) (x_max 1) (y_min 0) (y_max 1) (z_min 0) (z_max 1)) |}];
  print_s [%sexp (Bb.octant_bb bb Bb.O2 : Bb.t)];
  [%expect {| ((x_min -1) (x_max 0) (y_min 0) (y_max 1) (z_min 0) (z_max 1)) |}];
  print_s [%sexp (Bb.octant_bb bb Bb.O3 : Bb.t)];
  [%expect {| ((x_min 0) (x_max 1) (y_min -1) (y_max 0) (z_min 0) (z_max 1)) |}];
  print_s [%sexp (Bb.octant_bb bb Bb.O4 : Bb.t)];
  [%expect {| ((x_min -1) (x_max 0) (y_min -1) (y_max 0) (z_min 0) (z_max 1)) |}];
  print_s [%sexp (Bb.octant_bb bb Bb.O5 : Bb.t)];
  [%expect {| ((x_min 0) (x_max 1) (y_min 0) (y_max 1) (z_min -1) (z_max 0)) |}];
  print_s [%sexp (Bb.octant_bb bb Bb.O6 : Bb.t)];
  [%expect {| ((x_min -1) (x_max 0) (y_min 0) (y_max 1) (z_min -1) (z_max 0)) |}];
  print_s [%sexp (Bb.octant_bb bb Bb.O7 : Bb.t)];
  [%expect {| ((x_min 0) (x_max 1) (y_min -1) (y_max 0) (z_min -1) (z_max 0)) |}];
  print_s [%sexp (Bb.octant_bb bb Bb.O8 : Bb.t)];
  [%expect {| ((x_min -1) (x_max 0) (y_min -1) (y_max 0) (z_min -1) (z_max 0)) |}]
;;

let%expect_test "parent_bb" =
  print_s [%sexp (Bb.parent_bb (Bb.octant_bb bb Bb.O1) Bb.O1 : Bb.t)];
  [%expect {| ((x_min -1) (x_max 1) (y_min -1) (y_max 1) (z_min -1) (z_max 1)) |}];
  print_s [%sexp (Bb.parent_bb (Bb.octant_bb bb Bb.O2) Bb.O2 : Bb.t)];
  [%expect {| ((x_min -1) (x_max 1) (y_min -1) (y_max 1) (z_min -1) (z_max 1)) |}];
  print_s [%sexp (Bb.parent_bb (Bb.octant_bb bb Bb.O3) Bb.O3 : Bb.t)];
  [%expect {| ((x_min -1) (x_max 1) (y_min -1) (y_max 1) (z_min -1) (z_max 1)) |}];
  print_s [%sexp (Bb.parent_bb (Bb.octant_bb bb Bb.O4) Bb.O4 : Bb.t)];
  [%expect {| ((x_min -1) (x_max 1) (y_min -1) (y_max 1) (z_min -1) (z_max 1)) |}];
  print_s [%sexp (Bb.parent_bb (Bb.octant_bb bb Bb.O5) Bb.O5 : Bb.t)];
  [%expect {| ((x_min -1) (x_max 1) (y_min -1) (y_max 1) (z_min -1) (z_max 1)) |}];
  print_s [%sexp (Bb.parent_bb (Bb.octant_bb bb Bb.O6) Bb.O6 : Bb.t)];
  [%expect {| ((x_min -1) (x_max 1) (y_min -1) (y_max 1) (z_min -1) (z_max 1)) |}];
  print_s [%sexp (Bb.parent_bb (Bb.octant_bb bb Bb.O7) Bb.O7 : Bb.t)];
  [%expect {| ((x_min -1) (x_max 1) (y_min -1) (y_max 1) (z_min -1) (z_max 1)) |}];
  print_s [%sexp (Bb.parent_bb (Bb.octant_bb bb Bb.O8) Bb.O8 : Bb.t)];
  [%expect {| ((x_min -1) (x_max 1) (y_min -1) (y_max 1) (z_min -1) (z_max 1)) |}];
  print_s [%sexp (Bb.parent_bb bb Bb.O1 : Bb.t)];
  [%expect {| ((x_min -3) (x_max 1) (y_min -3) (y_max 1) (z_min -3) (z_max 1)) |}];
  print_s [%sexp (Bb.parent_bb bb Bb.O2 : Bb.t)];
  [%expect {| ((x_min -1) (x_max 3) (y_min -3) (y_max 1) (z_min -3) (z_max 1)) |}];
  print_s [%sexp (Bb.parent_bb bb Bb.O3 : Bb.t)];
  [%expect {| ((x_min -3) (x_max 1) (y_min -1) (y_max 3) (z_min -3) (z_max 1)) |}];
  print_s [%sexp (Bb.parent_bb bb Bb.O4 : Bb.t)];
  [%expect {| ((x_min -1) (x_max 3) (y_min -1) (y_max 3) (z_min -3) (z_max 1)) |}];
  print_s [%sexp (Bb.parent_bb bb Bb.O5 : Bb.t)];
  [%expect {| ((x_min -3) (x_max 1) (y_min -3) (y_max 1) (z_min -1) (z_max 3)) |}];
  print_s [%sexp (Bb.parent_bb bb Bb.O6 : Bb.t)];
  [%expect {| ((x_min -1) (x_max 3) (y_min -3) (y_max 1) (z_min -1) (z_max 3)) |}];
  print_s [%sexp (Bb.parent_bb bb Bb.O7 : Bb.t)];
  [%expect {| ((x_min -3) (x_max 1) (y_min -1) (y_max 3) (z_min -1) (z_max 3)) |}];
  print_s [%sexp (Bb.parent_bb bb Bb.O8 : Bb.t)];
  [%expect {| ((x_min -1) (x_max 3) (y_min -1) (y_max 3) (z_min -1) (z_max 3)) |}]
;;

let%expect_test "octant_of_point" =
  print_s [%sexp (Bb.octant_of_point (Physics.point 0.5 0.5 0.5) bb : Bb.octant)];
  [%expect {| O1 |}];
  print_s [%sexp (Bb.octant_of_point (Physics.point (-0.5) 0.5 0.5) bb : Bb.octant)];
  [%expect {| O2 |}];
  print_s [%sexp (Bb.octant_of_point (Physics.point 0.5 (-0.5) 0.5) bb : Bb.octant)];
  [%expect {| O3 |}];
  print_s [%sexp (Bb.octant_of_point (Physics.point (-0.5) (-0.5) 0.5) bb : Bb.octant)];
  [%expect {| O4 |}];
  print_s [%sexp (Bb.octant_of_point (Physics.point 0.5 0.5 (-0.5)) bb : Bb.octant)];
  [%expect {| O5 |}];
  print_s [%sexp (Bb.octant_of_point (Physics.point (-0.5) 0.5 (-0.5)) bb : Bb.octant)];
  [%expect {| O6 |}];
  print_s [%sexp (Bb.octant_of_point (Physics.point 0.5 (-0.5) (-0.5)) bb : Bb.octant)];
  [%expect {| O7 |}];
  print_s [%sexp (Bb.octant_of_point (Physics.point (-0.5) (-0.5) (-0.5)) bb : Bb.octant)];
  [%expect {| O8 |}]
;;

let%expect_test "centroid add" =
  let c1 = C.{ p = Physics.point 1. 1. 1.; m = 1. } in
  let c2 = C.{ p = Physics.point 0. 0. 0.; m = 2. } in
  let c = C.add c1 c2 in
  let open Physics in
  printf "x: %f; y: %f; z: %f; m %f\n" c.p.%{`x} c.p.%{`y} c.p.%{`z} c.m;
  [%expect {| x: 0.333333; y: 0.333333; z: 0.333333; m 3.000000 |}]
;;

let%expect_test "has_children" =
  let children = Int32.[| [| zero; zero; zero; zero; zero; zero; zero; zero |] |] in
  let children = Bigarray.Array2.of_array Bigarray.int32 Bigarray.c_layout children in
  let node_index = 0 in
  Otree.has_children children node_index |> [%sexp_of: bool] |> print_s;
  [%expect {| false |}];
  let children =
    Int32.[| [| zero; Int32.of_int_exn 1; zero; zero; zero; zero; zero; zero |] |]
  in
  let children = Bigarray.Array2.of_array Bigarray.int32 Bigarray.c_layout children in
  let node_index = 0 in
  Otree.has_children children node_index |> [%sexp_of: bool] |> print_s;
  [%expect {| true |}]
;;

let%expect_test "insert_tree" =
  let tree = Otree.create_tree ~capacity:10 ~theta:0.5 in
  Otree.print_tree tree;
  [%expect
    {|
    Tree:
    size: 0
    capacity: 10
    centroids:
    children:
    |}];
  let bb =
    Bb.{ x_min = -1.; x_max = 1.; y_min = -1.; y_max = 1.; z_min = -1.; z_max = 1. }
  in
  let c = C.{ p = Physics.point 0.5 0.5 0.6; m = 1. } in
  Otree.insert_body tree c bb;
  Otree.print_tree tree;
  [%expect
    {|
    Tree:
    size: 1
    capacity: 10
    centroids:
    0)	P: x: 5.0000e-01 y: 5.0000e-01 z: 6.0000e-01 M: 1.00
    children:
    0)	0-0-0-0-0-0-0-0-
    |}];
  let c = C.{ p = Physics.point 0.5 0.5 (-0.6); m = 1. } in
  Otree.insert_body tree c bb;
  Otree.print_tree tree;
  [%expect
    {|
    Tree:
    size: 3
    capacity: 10
    centroids:
    0)	P: x: 5.0000e-01 y: 5.0000e-01 z: 6.0000e-01 M: 1.00
    1)	P: x: 5.0000e-01 y: 5.0000e-01 z: 6.0000e-01 M: 1.00
    2)	P: x: 5.0000e-01 y: 5.0000e-01 z: -6.0000e-01 M: 1.00
    children:
    0)	1-0-0-0-2-0-0-0-
    1)	0-0-0-0-0-0-0-0-
    2)	0-0-0-0-0-0-0-0-
    |}];
  let c = C.{ p = Physics.point 0.5 0.5 (-0.7); m = 1. } in
  Otree.insert_body tree c bb;
  Otree.print_tree tree;
  [%expect
    {|
    Tree:
    size: 5
    capacity: 10
    centroids:
    0)	P: x: 5.0000e-01 y: 5.0000e-01 z: -5.0000e-02 M: 2.00
    1)	P: x: 5.0000e-01 y: 5.0000e-01 z: 6.0000e-01 M: 1.00
    2)	P: x: 5.0000e-01 y: 5.0000e-01 z: -6.0000e-01 M: 1.00
    3)	P: x: 5.0000e-01 y: 5.0000e-01 z: -6.0000e-01 M: 1.00
    4)	P: x: 5.0000e-01 y: 5.0000e-01 z: -7.0000e-01 M: 1.00
    children:
    0)	1-0-0-0-2-0-0-0-
    1)	0-0-0-0-0-0-0-0-
    2)	0-0-0-0-4-0-0-0-
    3)	0-0-0-0-0-0-0-0-
    4)	0-0-0-0-0-0-0-0-
    |}]
;;

let%expect_test "barnes_hut_force_calculation" =
  let bounds =
    Bb.
      { x_min = -100.0
      ; x_max = 100.0
      ; y_min = -100.0
      ; y_max = 100.0
      ; z_min = -100.0
      ; z_max = 100.0
      }
  in
  let tree = Otree.create_tree ~capacity:10 ~theta:0.5 in
  
  (* Create some test bodies *)
  let open Physics in
  let bodies =
    Body.
      [ { pos = point 0. 0. 0.; mass = 1000.0; vel = vec 0. 0. 0. }
      ; { pos = point 50. 0. 0.; mass = 1000.0; vel = vec 0. 10. 0. }
      ; { pos = point (-50.) 0. 0.; mass = 1000.0; vel = vec 0. (-10.) 0. }
      ]
  in
  
  (* Insert bodies into tree *)
  let centroids =
    List.map bodies ~f:(fun (body : Body.t) -> C.{ m = body.mass; p = body.pos })
  in
  List.iter centroids ~f:(fun c -> Otree.insert_body tree c bounds);
  
  Printf.printf "Tree after inserting bodies:\n";
  Otree.print_tree tree;
  
  (* Calculate forces *)
  let forces = Otree.calculate_all_forces tree bodies bounds in
  List.iteri forces ~f:(fun i (_, force) ->
    Printf.printf "Body %d: Force = [%g, %g, %g]\n" i force.%{`x} force.%{`y} force.%{`z});
  
  (* Verify that forces are reasonable (should be attracting towards each other) *)
  let _, force0 = List.nth_exn forces 0 in
  let _, force1 = List.nth_exn forces 1 in  
  let _, force2 = List.nth_exn forces 2 in
  
  (* Body at origin should experience near-zero net force due to symmetry *)
  Printf.printf "Net force on central body: %g (should be close to 0)\n" (Physics.mag force0);
  
  (* Bodies on sides should be attracted toward center *)
  Printf.printf "Force on right body (should be negative x): %g\n" force1.%{`x};
  Printf.printf "Force on left body (should be positive x): %g\n" force2.%{`x};
  
  [%expect {|
    Tree after inserting bodies:
    Tree:
    size: 4
    capacity: 10
    centroids:
    0)	P: x: -2.5000e+01 y: 0.0000e+00 z: 0.0000e+00 M: 2000.00
    1)	P: x: 0.0000e+00 y: 0.0000e+00 z: 0.0000e+00 M: 1000.00
    2)	P: x: 5.0000e+01 y: 0.0000e+00 z: 0.0000e+00 M: 1000.00
    3)	P: x: -5.0000e+01 y: 0.0000e+00 z: 0.0000e+00 M: 1000.00
    children:
    0)	2-3-0-0-0-0-0-0-
    1)	0-0-0-0-0-0-0-0-
    2)	0-0-0-0-0-0-0-0-
    3)	0-0-0-0-0-0-0-0-
    Body 0: Force = [0, 0, 0]
    Body 1: Force = [-6.67428e-12, 0, 0]
    Body 2: Force = [6.67428e-12, 0, 0]
    Net force on central body: 0 (should be close to 0)
    Force on right body (should be negative x): -6.67428e-12
    Force on left body (should be positive x): 6.67428e-12
    |}]
;;

let%expect_test "simulation_timestep" =
  let bounds =
    Bb.
      { x_min = -1000.0
      ; x_max = 1000.0
      ; y_min = -1000.0
      ; y_max = 1000.0
      ; z_min = -1000.0
      ; z_max = 1000.0
      }
  in
  
  (* Create a simple two-body system *)
  let open Physics in
  let bodies =
    Body.
      [ { pos = point 100. 0. 0.; mass = 1000.0; vel = vec 0. 5. 0. }
      ; { pos = point (-100.) 0. 0.; mass = 1000.0; vel = vec 0. (-5.) 0. }
      ]
  in
  
  Printf.printf "Initial positions:\n";
  List.iteri bodies ~f:(fun i body ->
    Printf.printf "Body %d: pos = [%g, %g, %g], vel = [%g, %g, %g]\n" 
      i body.pos.%{`x} body.pos.%{`y} body.pos.%{`z}
      body.vel.%{`x} body.vel.%{`y} body.vel.%{`z});
  
  (* Run one simulation timestep *)
  Otree.simulation_timestep bodies bounds ~theta:0.5 ~dt:0.1;
  
  Printf.printf "\nPositions after timestep:\n";
  List.iteri bodies ~f:(fun i body ->
    Printf.printf "Body %d: pos = [%g, %g, %g], vel = [%g, %g, %g]\n" 
      i body.pos.%{`x} body.pos.%{`y} body.pos.%{`z}
      body.vel.%{`x} body.vel.%{`y} body.vel.%{`z});
  
  [%expect {|
    Initial positions:
    Body 0: pos = [100, 0, 0], vel = [0, 5, 0]
    Body 1: pos = [-100, 0, 0], vel = [0, -5, 0]

    Positions after timestep:
    Body 0: pos = [100, 0.5, 0], vel = [0, 5, 0]
    Body 1: pos = [-100, -0.5, 0], vel = [1.66857e-16, -5, 0]
    |}]
;;

let%expect_test "single_body_force" =
  let bounds = Bb.{ x_min = -10.; x_max = 10.; y_min = -10.; y_max = 10.; z_min = -10.; z_max = 10. } in
  let tree = Otree.create_tree ~capacity:5 ~theta:0.5 in
  let open Physics in
  let body = Body.{ pos = point 1. 2. 3.; mass = 100.0; vel = vec 0. 0. 0. } in
  
  let centroid = C.{ m = body.mass; p = body.pos } in
  Otree.insert_body tree centroid bounds;
  
  let force = Otree.calculate_force_on_body body tree bounds in
  Printf.printf "Force on single body: [%g, %g, %g]\n" force.%{`x} force.%{`y} force.%{`z};
  Printf.printf "Force magnitude: %g (should be 0)\n" (Physics.mag force);
  
  [%expect {|
    Force on single body: [0, 0, 0]
    Force magnitude: 0 (should be 0)
    |}]
;;

let%expect_test "identical_positions" =
  let bounds = Bb.{ x_min = -10.; x_max = 10.; y_min = -10.; y_max = 10.; z_min = -10.; z_max = 10. } in
  let tree = Otree.create_tree ~capacity:5 ~theta:0.5 in
  let open Physics in
  let bodies = Body.[
    { pos = point 1. 1. 1.; mass = 100.0; vel = vec 0. 0. 0. };
    { pos = point 1. 1. 1.; mass = 200.0; vel = vec 0. 0. 0. };
  ] in
  
  let centroids = List.map bodies ~f:(fun body -> C.{ m = body.mass; p = body.pos }) in
  List.iter centroids ~f:(fun c -> Otree.insert_body tree c bounds);
  
  Printf.printf "Tree with identical positions:\n";
  Otree.print_tree tree;
  
  let forces = Otree.calculate_all_forces tree bodies bounds in
  List.iteri forces ~f:(fun i (_, force) ->
    Printf.printf "Body %d force: [%g, %g, %g]\n" i force.%{`x} force.%{`y} force.%{`z});
  
  [%expect {|
    Tree with identical positions:
    Tree:
    size: 3
    capacity: 5
    centroids:
    0)	P: x: 1.0000e+00 y: 1.0000e+00 z: 1.0000e+00 M: 100.00
    1)	P: x: 1.0000e+00 y: 1.0000e+00 z: 1.0000e+00 M: 100.00
    2)	P: x: 1.0000e+00 y: 1.0000e+00 z: 1.0000e+00 M: 200.00
    children:
    0)	2-0-0-0-0-0-0-0-
    1)	0-0-0-0-0-0-0-0-
    2)	0-0-0-0-0-0-0-0-
    Body 0 force: [0, 0, 0]
    Body 1 force: [0, 0, 0]
    |}]
;;

let%expect_test "different_theta_values" =
  let bounds = Bb.{ x_min = -100.; x_max = 100.; y_min = -100.; y_max = 100.; z_min = -100.; z_max = 100. } in
  let open Physics in
  let bodies = Body.[
    { pos = point 0. 0. 0.; mass = 1000.0; vel = vec 0. 0. 0. };
    { pos = point 10. 0. 0.; mass = 1000.0; vel = vec 0. 0. 0. };
    { pos = point 20. 0. 0.; mass = 1000.0; vel = vec 0. 0. 0. };
  ] in
  
  (* Test with high precision (small theta) *)
  let tree_precise = Otree.create_tree ~capacity:10 ~theta:0.1 in
  let centroids = List.map bodies ~f:(fun body -> C.{ m = body.mass; p = body.pos }) in
  List.iter centroids ~f:(fun c -> Otree.insert_body tree_precise c bounds);
  let forces_precise = Otree.calculate_all_forces tree_precise bodies bounds in
  let _, force_precise = List.nth_exn forces_precise 1 in
  
  (* Test with low precision (large theta) *)
  let tree_approx = Otree.create_tree ~capacity:10 ~theta:2.0 in
  let centroids = List.map bodies ~f:(fun body -> C.{ m = body.mass; p = body.pos }) in
  List.iter centroids ~f:(fun c -> Otree.insert_body tree_approx c bounds);
  let forces_approx = Otree.calculate_all_forces tree_approx bodies bounds in
  let _, force_approx = List.nth_exn forces_approx 1 in
  
  Printf.printf "Force on middle body (theta=0.1): [%g, %g, %g]\n" 
    force_precise.%{`x} force_precise.%{`y} force_precise.%{`z};
  Printf.printf "Force on middle body (theta=2.0): [%g, %g, %g]\n" 
    force_approx.%{`x} force_approx.%{`y} force_approx.%{`z};
  Printf.printf "Force difference: %g\n" 
    (Physics.mag (Owl.Mat.(force_precise - force_approx)));
  
  [%expect {|
    Force on middle body (theta=0.1): [0, 0, 0]
    Force on middle body (theta=2.0): [0, 0, 0]
    Force difference: 0
    |}]
;;

let%expect_test "tree_growth" =
  let bounds = Bb.{ x_min = -10.; x_max = 10.; y_min = -10.; y_max = 10.; z_min = -10.; z_max = 10. } in
  let tree = Otree.create_tree ~capacity:2 ~theta:0.5 in
  let open Physics in
  Printf.printf "Initial tree capacity: %d\n" tree.capacity;
  
  (* Insert bodies to trigger growth *)
  let bodies = [
    C.{ m = 100.; p = point 1. 1. 1. };
    C.{ m = 200.; p = point 2. 2. 2. };
    C.{ m = 300.; p = point 3. 3. 3. };
  ] in
  
  List.iteri bodies ~f:(fun i body ->
    Otree.insert_body tree body bounds;
    Printf.printf "After inserting body %d: capacity=%d, size=%d\n" i tree.capacity tree.size);
  
  [%expect {|
    Initial tree capacity: 2
    After inserting body 0: capacity=2, size=1
    After inserting body 1: capacity=4, size=3
    After inserting body 2: capacity=8, size=5
    |}]
;;

let%expect_test "3d_scenario" =
  let bounds = Bb.{ x_min = -50.; x_max = 50.; y_min = -50.; y_max = 50.; z_min = -50.; z_max = 50. } in
  let tree = Otree.create_tree ~capacity:10 ~theta:0.5 in
  let open Physics in
  (* Create bodies in 3D space (not on same plane) *)
  let bodies = Body.[
    { pos = point 10. 0. 0.; mass = 1000.0; vel = vec 0. 0. 0. };
    { pos = point 0. 10. 0.; mass = 1000.0; vel = vec 0. 0. 0. };
    { pos = point 0. 0. 10.; mass = 1000.0; vel = vec 0. 0. 0. };
    { pos = point (-5.) (-5.) (-5.); mass = 1000.0; vel = vec 0. 0. 0. };
  ] in
  
  let centroids = List.map bodies ~f:(fun body -> C.{ m = body.mass; p = body.pos }) in
  List.iter centroids ~f:(fun c -> Otree.insert_body tree c bounds);
  
  Printf.printf "3D tree structure:\n";
  Otree.print_tree tree;
  
  let forces = Otree.calculate_all_forces tree bodies bounds in
  List.iteri forces ~f:(fun i (_, force) ->
    Printf.printf "Body %d force magnitude: %g\n" i (Physics.mag force));
  
  [%expect {|
    3D tree structure:
    Tree:
    size: 6
    capacity: 10
    centroids:
    0)	P: x: 1.6667e+00 y: -1.6667e+00 z: 1.6667e+00 M: 3000.00
    1)	P: x: 1.0000e+01 y: 0.0000e+00 z: 0.0000e+00 M: 1000.00
    2)	P: x: 0.0000e+00 y: 1.0000e+01 z: 0.0000e+00 M: 1000.00
    3)	P: x: 0.0000e+00 y: 1.0000e+01 z: 0.0000e+00 M: 1000.00
    4)	P: x: 0.0000e+00 y: 0.0000e+00 z: 1.0000e+01 M: 1000.00
    5)	P: x: -5.0000e+00 y: -5.0000e+00 z: -5.0000e+00 M: 1000.00
    children:
    0)	2-0-0-0-0-0-0-5-
    1)	0-0-0-0-0-0-0-0-
    2)	0-0-0-0-0-0-0-4-
    3)	0-0-0-0-0-0-0-0-
    4)	0-0-0-0-0-0-0-0-
    5)	0-0-0-0-0-0-0-0-
    Body 0 force magnitude: 4.89224e-10
    Body 1 force magnitude: 2.42701e-10
    Body 2 force magnitude: 2.42701e-10
    Body 3 force magnitude: 2.42701e-10
    |}]
;;

let%expect_test "large_mass_difference" =
  let bounds = Bb.{ x_min = -100.; x_max = 100.; y_min = -100.; y_max = 100.; z_min = -100.; z_max = 100. } in
  let tree = Otree.create_tree ~capacity:5 ~theta:0.5 in
  let open Physics in
  (* Create bodies with very different masses *)
  let bodies = Body.[
    { pos = point (-10.) 0. 0.; mass = 1e9; vel = vec 0. 0. 0. };  (* Massive *)
    { pos = point 10. 0. 0.; mass = 1e-3; vel = vec 0. 0. 0. };    (* Tiny *)
  ] in
  
  let centroids = List.map bodies ~f:(fun body -> C.{ m = body.mass; p = body.pos }) in
  List.iter centroids ~f:(fun c -> Otree.insert_body tree c bounds);
  
  let forces = Otree.calculate_all_forces tree bodies bounds in
  let _, force_massive = List.nth_exn forces 0 in
  let _, force_tiny = List.nth_exn forces 1 in
  
  Printf.printf "Force on massive body: magnitude = %g\n" (Physics.mag force_massive);
  Printf.printf "Force on tiny body: magnitude = %g\n" (Physics.mag force_tiny);
  Printf.printf "Force ratio (tiny/massive): %g\n" 
    ((Physics.mag force_tiny) /. (Physics.mag force_massive));
  
  [%expect {|
    Force on massive body: magnitude = 0
    Force on tiny body: magnitude = 0.000166857
    Force ratio (tiny/massive): inf
    |}]
;;

let%expect_test "zero_mass_body" =
  let bounds = Bb.{ x_min = -10.; x_max = 10.; y_min = -10.; y_max = 10.; z_min = -10.; z_max = 10. } in
  let tree = Otree.create_tree ~capacity:5 ~theta:0.5 in
  let open Physics in
  let bodies = Body.[
    { pos = point 1. 0. 0.; mass = 100.0; vel = vec 0. 0. 0. };
    { pos = point (-1.) 0. 0.; mass = 0.0; vel = vec 0. 0. 0. };
  ] in
  
  let centroids = List.map bodies ~f:(fun body -> C.{ m = body.mass; p = body.pos }) in
  List.iter centroids ~f:(fun c -> Otree.insert_body tree c bounds);
  
  Printf.printf "Tree with zero mass body:\n";
  Otree.print_tree tree;
  
  let forces = Otree.calculate_all_forces tree bodies bounds in
  List.iteri forces ~f:(fun i (_, force) ->
    Printf.printf "Body %d force: [%g, %g, %g]\n" i force.%{`x} force.%{`y} force.%{`z});
  
  [%expect {|
    Tree with zero mass body:
    Tree:
    size: 3
    capacity: 5
    centroids:
    0)	P: x: 1.0000e+00 y: 0.0000e+00 z: 0.0000e+00 M: 100.00
    1)	P: x: 1.0000e+00 y: 0.0000e+00 z: 0.0000e+00 M: 100.00
    2)	P: x: -1.0000e+00 y: 0.0000e+00 z: 0.0000e+00 M: 0.00
    children:
    0)	1-2-0-0-0-0-0-0-
    1)	0-0-0-0-0-0-0-0-
    2)	0-0-0-0-0-0-0-0-
    Body 0 force: [0, 0, 0]
    Body 1 force: [1.66857e-09, 0, 0]
    |}]
;;

let%expect_test "performance_comparison" =
  let bounds = Bb.{ x_min = -100.; x_max = 100.; y_min = -100.; y_max = 100.; z_min = -100.; z_max = 100. } in
  let open Physics in
  (* Create multiple bodies for performance test *)
  let create_bodies n =
    List.init n ~f:(fun i ->
      let angle = Float.(2. * pi * of_int i / of_int n) in
      let radius = 50. in
      Body.{
        pos = point 
          (radius *. Float.cos angle) 
          (radius *. Float.sin angle) 
          (Float.of_int i *. 2.);
        mass = 1000.0 +. Float.of_int i;
        vel = vec 0. 0. 0.
      })
  in
  
  let bodies_small = create_bodies 3 in
  let bodies_medium = create_bodies 5 in
  
  (* Test with small number of bodies *)
  let tree_small = Otree.create_tree ~capacity:20 ~theta:0.5 in
  let centroids_small = List.map bodies_small ~f:(fun body -> C.{ m = body.mass; p = body.pos }) in
  List.iter centroids_small ~f:(fun c -> Otree.insert_body tree_small c bounds);
  let forces_small = Otree.calculate_all_forces tree_small bodies_small bounds in
  
  (* Test with medium number of bodies *)
  let tree_medium = Otree.create_tree ~capacity:30 ~theta:0.5 in
  let centroids_medium = List.map bodies_medium ~f:(fun body -> C.{ m = body.mass; p = body.pos }) in
  List.iter centroids_medium ~f:(fun c -> Otree.insert_body tree_medium c bounds);
  let forces_medium = Otree.calculate_all_forces tree_medium bodies_medium bounds in
  
  Printf.printf "Small test (%d bodies): tree size = %d\n" 
    (List.length bodies_small) tree_small.size;
  Printf.printf "Medium test (%d bodies): tree size = %d\n" 
    (List.length bodies_medium) tree_medium.size;
  
  Printf.printf "Average force magnitude (small): %g\n"
    (List.fold forces_small ~init:0. ~f:(fun acc (_, force) -> 
      acc +. mag force) /. Float.of_int (List.length forces_small));
  Printf.printf "Average force magnitude (medium): %g\n"
    (List.fold forces_medium ~init:0. ~f:(fun acc (_, force) -> 
      acc +. mag force) /. Float.of_int (List.length forces_medium));
  
  [%expect {|
    Small test (3 bodies): tree size = 4
    Medium test (5 bodies): tree size = 6
    Average force magnitude (small): 1.54126e-11
    Average force magnitude (medium): 3.12049e-11
    |}]
;;
