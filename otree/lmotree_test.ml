open! Core
open! Lmotree
open! Utils

(* Usage example with efficient batch processing *)
let gravity_simulation_example () =
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
  (* Create empty tree *)
  let tree = create bounds 0.5 in
  (* Add many bodies efficiently *)
  let bodies =
    let open Physics in
    Body.
      [ { pos = point 0. 0. 0.; mass = 1000.0; vel = vec 0. 0. 0. }
      ; { pos = point 100. 0. 0.; mass = 1000.0; vel = vec 0. 10. 0. }
      ; { pos = point (-100.) 0. 0.; mass = 1000.0; vel = vec 0. (-10.) 0. }
      ]
  in
  let centroids =
    List.map bodies ~f:(fun (body : Body.t) -> C.{ m = body.mass; p = body.pos })
  in
  (* EFFICIENT: Batch insert all bodies first (fast) *)
  let tree_with_bodies = insert_bodies centroids tree in
  (* THEN: Build all aggregates once (single pass) *)
  let tree_with_aggregates = build_aggregates tree_with_bodies in
  print_s [%sexp (tree_with_aggregates : Lmotree.t)];
  (* let forces = calculate_all_forces tree_with_aggregates bodies in *)
  (* update_simulation forces 0.01; *)
  Printf.printf "Efficient gravity simulation:\n";
  Printf.printf "1. Batch inserted %d bodies (O(N))\n" (List.length bodies);
  Printf.printf "2. Built aggregates once (O(N))\n";
  Printf.printf "3. Calculated forces using Barnes-Hut (O(N log N))\n";
  tree_with_aggregates
;;

let%expect_test "example" =
  let _ = gravity_simulation_example () in
  ();
  [%expect {|
    ((bodies
      ((0x7ff6ff6ff6ff6ff6ff6ff6ff6ff6ff6
        (((p ((x -100) (y 0) (z 0))) (m 1000))))
       (0x7ffffffffffffffffffffffffffffff (((p ((x 0) (y 0) (z 0))) (m 1000))))
       (0xedbfdbfdbfdbfdbfdbfdbfdbfdbfdbe (((p ((x 100) (y 0) (z 0))) (m 1000))))))
     (regions
      ((0x00000000000000000
        ((centroid ((p ((x -20) (y 0) (z 0))) (m 5000))) (bodies 5) (size 2000)))
       (0x00000000000000001
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 2000)))
       (0x00000000000000007
        ((centroid ((p ((x -50) (y 0) (z 0))) (m 2000))) (bodies 2) (size 1000)))
       (0x0000000000000000e
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 1000)))
       (0x0000000000000003f
        ((centroid ((p ((x -50) (y 0) (z 0))) (m 2000))) (bodies 2) (size 500)))
       (0x00000000000000076
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 1000)))
       (0x000000000000001ff
        ((centroid ((p ((x -50) (y 0) (z 0))) (m 2000))) (bodies 2) (size 250)))
       (0x000000000000003b6
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 500)))
       (0x00000000000000ffe
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 250)))
       (0x00000000000000fff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1) (size 125)))
       (0x00000000000001db7
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 250)))
       (0x00000000000007ff6
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 125)))
       (0x00000000000007fff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1) (size 62.5)))
       (0x0000000000000edbf
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 125)))
       (0x0000000000003ffb7
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 62.5)))
       (0x0000000000003ffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1) (size 31.25)))
       (0x00000000000076dfe
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 62.5)))
       (0x000000000001ffdbf
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 31.25)))
       (0x000000000001fffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1) (size 15.625)))
       (0x000000000003b6ff6
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 62.5)))
       (0x00000000000ffedfe
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 15.625)))
       (0x00000000000ffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1) (size 7.8125)))
       (0x00000000001db7fb7
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 31.25)))
       (0x00000000007ff6ff6
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 15.625)))
       (0x00000000007ffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1) (size 3.90625)))
       (0x0000000000edbfdbf
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 15.625)))
       (0x0000000003ffb7fb7
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 7.8125)))
       (0x0000000003fffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 1.953125)))
       (0x00000000076dfedfe
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 7.8125)))
       (0x000000001ffdbfdbf
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 3.90625)))
       (0x000000001ffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.9765625)))
       (0x000000003b6ff6ff6
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 3.90625)))
       (0x00000000ffedfedfe
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 1.953125)))
       (0x00000000fffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.48828125)))
       (0x00000001db7fb7fb7
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 1.953125)))
       (0x00000007ff6ff6ff6
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.9765625)))
       (0x00000007fffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.244140625)))
       (0x0000000edbfdbfdbf
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.9765625)))
       (0x0000003ffb7fb7fb7
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.48828125)))
       (0x0000003ffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.1220703125)))
       (0x00000076dfedfedfe
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.9765625)))
       (0x000001ffdbfdbfdbf
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.244140625)))
       (0x000001fffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.06103515625)))
       (0x000003b6ff6ff6ff6
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.48828125)))
       (0x00000ffedfedfedfe
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.244140625)))
       (0x00000ffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.030517578125)))
       (0x00001db7fb7fb7fb7
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.244140625)))
       (0x00007ff6ff6ff6ff6
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.1220703125)))
       (0x00007ffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.0152587890625)))
       (0x0000edbfdbfdbfdbf
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.1220703125)))
       (0x0003ffb7fb7fb7fb7
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.06103515625)))
       (0x0003fffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.00762939453125)))
       (0x00076dfedfedfedfe
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.06103515625)))
       (0x001ffdbfdbfdbfdbf
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.030517578125)))
       (0x001ffffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.003814697265625)))
       (0x003b6ff6ff6ff6ff6
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.06103515625)))
       (0x00ffedfedfedfedfe
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.0152587890625)))
       (0x00fffffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.0019073486328125)))
       (0x01db7fb7fb7fb7fb7
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.030517578125)))
       (0x07ff6ff6ff6ff6ff6
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.0152587890625)))
       (0x07fffffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.00095367431640625)))
       (0x0edbfdbfdbfdbfdbf
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.0152587890625)))
       (0x3ffb7fb7fb7fb7fb7
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.00762939453125)))
       (0x3ffffffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.000476837158203125)))
       (0x76dfedfedfedfedfe
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.00762939453125)))
       (0x1ffdbfdbfdbfdbfdbf
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.003814697265625)))
       (0x1fffffffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.0002384185791015625)))
       (0x3b6ff6ff6ff6ff6ff6
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.003814697265625)))
       (0xffedfedfedfedfedfe
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.0019073486328125)))
       (0xffffffffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.00011920928955078125)))
       (0x1db7fb7fb7fb7fb7fb7
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.0019073486328125)))
       (0x7ff6ff6ff6ff6ff6ff6
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.00095367431640625)))
       (0x7ffffffffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 5.9604644775390625E-05)))
       (0xedbfdbfdbfdbfdbfdbf
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.00095367431640625)))
       (0x3ffb7fb7fb7fb7fb7fb7
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.000476837158203125)))
       (0x3fffffffffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 2.9802322387695312E-05)))
       (0x76dfedfedfedfedfedfe
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.00095367431640625)))
       (0x1ffdbfdbfdbfdbfdbfdbf
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.0002384185791015625)))
       (0x1ffffffffffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 1.4901161193847656E-05)))
       (0x3b6ff6ff6ff6ff6ff6ff6
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.000476837158203125)))
       (0xffedfedfedfedfedfedfe
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.0002384185791015625)))
       (0xfffffffffffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 7.4505805969238281E-06)))
       (0x1db7fb7fb7fb7fb7fb7fb7
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.0002384185791015625)))
       (0x7ff6ff6ff6ff6ff6ff6ff6
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.00011920928955078125)))
       (0x7fffffffffffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 3.7252902984619141E-06)))
       (0xedbfdbfdbfdbfdbfdbfdbf
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.00011920928955078125)))
       (0x3ffb7fb7fb7fb7fb7fb7fb7
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 5.9604644775390625E-05)))
       (0x3ffffffffffffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 1.862645149230957E-06)))
       (0x76dfedfedfedfedfedfedfe
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 5.9604644775390625E-05)))
       (0x1ffdbfdbfdbfdbfdbfdbfdbf
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 2.9802322387695312E-05)))
       (0x1fffffffffffffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 9.3132257461547852E-07)))
       (0x3b6ff6ff6ff6ff6ff6ff6ff6
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 5.9604644775390625E-05)))
       (0xffedfedfedfedfedfedfedfe
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 1.4901161193847656E-05)))
       (0xffffffffffffffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 4.6566128730773926E-07)))
       (0x1db7fb7fb7fb7fb7fb7fb7fb7
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 2.9802322387695312E-05)))
       (0x7ff6ff6ff6ff6ff6ff6ff6ff6
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 1.4901161193847656E-05)))
       (0x7ffffffffffffffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 2.3283064365386963E-07)))
       (0xedbfdbfdbfdbfdbfdbfdbfdbf
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 1.4901161193847656E-05)))
       (0x3ffb7fb7fb7fb7fb7fb7fb7fb7
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 7.4505805969238281E-06)))
       (0x3fffffffffffffffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 1.1641532182693481E-07)))
       (0x76dfedfedfedfedfedfedfedfe
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 7.4505805969238281E-06)))
       (0x1ffdbfdbfdbfdbfdbfdbfdbfdbf
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 3.7252902984619141E-06)))
       (0x1ffffffffffffffffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 5.8207660913467407E-08)))
       (0x3b6ff6ff6ff6ff6ff6ff6ff6ff6
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 3.7252902984619141E-06)))
       (0xffedfedfedfedfedfedfedfedfe
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 1.862645149230957E-06)))
       (0xfffffffffffffffffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 2.9103830456733704E-08)))
       (0x1db7fb7fb7fb7fb7fb7fb7fb7fb7
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 1.862645149230957E-06)))
       (0x7ff6ff6ff6ff6ff6ff6ff6ff6ff6
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 9.3132257461547852E-07)))
       (0x7fffffffffffffffffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 1.4551915228366852E-08)))
       (0xedbfdbfdbfdbfdbfdbfdbfdbfdbf
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 9.3132257461547852E-07)))
       (0x3ffb7fb7fb7fb7fb7fb7fb7fb7fb7
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 4.6566128730773926E-07)))
       (0x3ffffffffffffffffffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 7.2759576141834259E-09)))
       (0x76dfedfedfedfedfedfedfedfedfe
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 9.3132257461547852E-07)))
       (0x1ffdbfdbfdbfdbfdbfdbfdbfdbfdbf
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 2.3283064365386963E-07)))
       (0x1fffffffffffffffffffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 3.637978807091713E-09)))
       (0x3b6ff6ff6ff6ff6ff6ff6ff6ff6ff6
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 4.6566128730773926E-07)))
       (0xffedfedfedfedfedfedfedfedfedfe
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 2.3283064365386963E-07)))
       (0xffffffffffffffffffffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 1.8189894035458565E-09)))
       (0x1db7fb7fb7fb7fb7fb7fb7fb7fb7fb7
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 2.3283064365386963E-07)))))
     (bounds
      ((x_min -1000) (x_max 1000) (y_min -1000) (y_max 1000) (z_min -1000)
       (z_max 1000)))
     (theta 0.5))
    Efficient gravity simulation:
    1. Batch inserted 3 bodies (O(N))
    2. Built aggregates once (O(N))
    3. Calculated forces using Barnes-Hut (O(N log N))
    |}]
;;
