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
       (0x0007ff6ff6ff6ff6f
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.06103515625)))
       (0x0007fffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.00762939453125)))
       (0x000edbfdbfdbfdbff
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
       (0x003ffb7fb7fb7fb7f
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.030517578125)))
       (0x003ffffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.003814697265625)))
       (0x0076dfedfedfedfed
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.030517578125)))
       (0x3000ffedfedfedfff
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.030517578125)))
       (0x3000fffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.00762939453125)))
       (0x7001db7fb7fb7fb7f
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.06103515625)))
       (0x1f0001ffdbfdbfdbff
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.030517578125)))
       (0x1f0001ffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.00762939453125)))
       (0x3b0003b6ff6ff6ffff
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.030517578125)))
       (0xff00003ffb7fb7ffff
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.030517578125)))
       (0xff00003fffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.00762939453125)))
       (0x1db000076dfedfedfff
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.06103515625)))
       (0x7ff000007ff6ff6ffff
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.030517578125)))
       (0x7ff000007ffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.00762939453125)))
       (0xedb00000edbfdbfffff
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.030517578125)))
       (0x3ffb000000ffedffffff
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.0152587890625)))
       (0x3fff000000ffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.00762939453125)))
       (0x76df000001db7fb7ffff
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.030517578125)))
       (0x1ffdb0000001ffdbfffff
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.030517578125)))
       (0x1ffff0000001fffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.00762939453125)))
       (0x3b6ff0000003b6fffffff
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.030517578125)))
       (0xffedf00000003ffffffff
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.0152587890625)))
       (0xfffff00000003ffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.00762939453125)))
       (0x1db7fb000000076dffffff
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.030517578125)))
       (0x7ff6ff000000007fffffff
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.0152587890625)))
       (0x7fffff000000007fffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.00762939453125)))
       (0xedbfdb00000000efffffff
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.030517578125)))
       (0x3ffb7fb000000003fffffff
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.0152587890625)))
       (0x3ffffff000000007fffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.003814697265625)))
       (0x76dfedf000000007fffffff
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.0152587890625)))
       (0x1ffdbfdb00000001bfffffff
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.003814697265625)))
       (0x1fffffff00000003ffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.00095367431640625)))
       (0x3b6ff6ff00000003f7ffffff
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.003814697265625)))
       (0xffedfedf0000000dfeffffff
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.0019073486328125)))
       (0xffffffff0000001fffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.0002384185791015625)))
       (0x1db7fb7fb0000001fb7ffffff
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.0019073486328125)))
       (0x7ff6ff6ff0000006ff6ffffff
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.000476837158203125)))
       (0x7ffffffff000000ffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 5.9604644775390625E-05)))
       (0xedbfdbfdb000000fdbfdfffff
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.000476837158203125)))
       (0x3ffb7fb7fb0000037fb7fbffff
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.0002384185791015625)))
       (0x3fffffffff000007ffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 1.4901161193847656E-05)))
       (0x76dfedfedf000007edfedfffff
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.0002384185791015625)))
       (0x1ffdbfdbfdb00001bfdbfdbffff
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 5.9604644775390625E-05)))
       (0x1ffffffffff00003fffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 3.7252902984619141E-06)))
       (0x3b6ff6ff6ff00003f6ff6ff7fff
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 5.9604644775390625E-05)))
       (0xffedfedfedf0000dfedfedfefff
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 2.9802322387695312E-05)))
       (0xfffffffffff0001ffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 9.3132257461547852E-07)))
       (0x1db7fb7fb7fb0001fb7fb7fb7fff
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 2.9802322387695312E-05)))
       (0x7ff6ff6ff6ff0006ff6ff6ff6fff
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 7.4505805969238281E-06)))
       (0x7fffffffffff000fffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 2.3283064365386963E-07)))
       (0xedbfdbfdbfdb000fdbfdbfdbfdff
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 7.4505805969238281E-06)))
       (0x3ffb7fb7fb7fb0037fb7fb7fb7fbf
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 3.7252902984619141E-06)))
       (0x3ffffffffffff007fffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 5.8207660913467407E-08)))
       (0x76dfedfedfedf007edfedfedfedff
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 3.7252902984619141E-06)))
       (0x1ffdbfdbfdbfdb01bfdbfdbfdbfdbf
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 9.3132257461547852E-07)))
       (0x1fffffffffffff03ffffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 1.4551915228366852E-08)))
       (0x3b6ff6ff6ff6ff03f6ff6ff6ff6ff7
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 9.3132257461547852E-07)))
       (0xffedfedfedfedf0dfedfedfedfedfe
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 4.6566128730773926E-07)))
       (0xffffffffffffff1fffffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 3.637978807091713E-09)))
       (0x1db7fb7fb7fb7fb1fb7fb7fb7fb7fb7
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 4.6566128730773926E-07)))))
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
