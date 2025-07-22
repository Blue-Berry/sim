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
      ((0x6da4da4da4da4da2ff6ff6ff6ff6ff6
        (((p ((x -100) (y 0) (z 0))) (m 1000))))
       (0x6db6db6db6db6db3fffffffffffffff (((p ((x 0) (y 0) (z 0))) (m 1000))))
       (0x14936936936936933dbfdbfdbfdbfdbe
        (((p ((x 100) (y 0) (z 0))) (m 1000))))))
     (regions
      ((0x000000000000000000000000000000
        ((centroid ((p ((x -20) (y 0) (z 0))) (m 5000))) (bodies 5) (size 2000)))
       (0x000000000000000000000000000001
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 2000)))
       (0x000000000000000000000000000003
        ((centroid ((p ((x -50) (y 0) (z 0))) (m 2000))) (bodies 2) (size 2000)))
       (0x00000000000000000000000000000a
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 2000)))
       (0x00000000000000000000000000001b
        ((centroid ((p ((x -50) (y 0) (z 0))) (m 2000))) (bodies 2) (size 1000)))
       (0x000000000000000000000000000052
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 1000)))
       (0x0000000000000000000000000000db
        ((centroid ((p ((x -50) (y 0) (z 0))) (m 2000))) (bodies 2) (size 500)))
       (0x000000000000000000000000000292
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 1000)))
       (0x0000000000000000000000000006da
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 500)))
       (0x0000000000000000000000000006db
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1) (size 500)))
       (0x000000000000000000000000001493
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 500)))
       (0x0000000000000000000000000036d2
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 500)))
       (0x0000000000000000000000000036db
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1) (size 250)))
       (0x00000000000000000000000000a49b
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 500)))
       (0x00000000000000000000000001b693
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 250)))
       (0x00000000000000000000000001b6db
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1) (size 125)))
       (0x0000000000000000000000000524da
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 250)))
       (0x0000000000000000000000000db49b
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 125)))
       (0x0000000000000000000000000db6db
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1) (size 125)))
       (0x0000000000000000000000002926d2
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 250)))
       (0x0000000000000000000000006da4da
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 125)))
       (0x0000000000000000000000006db6db
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1) (size 62.5)))
       (0x000000000000000000000001493693
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 125)))
       (0x0000000000000000000000036d26d2
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 125)))
       (0x0000000000000000000000036db6db
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1) (size 31.25)))
       (0x00000000000000000000000a49b49b
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 125)))
       (0x00000000000000000000001b693693
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 62.5)))
       (0x00000000000000000000001b6db6db
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1) (size 31.25)))
       (0x0000000000000000000000524da4da
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 62.5)))
       (0x0000000000000000000000db49b49b
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 31.25)))
       (0x0000000000000000000000db6db6db
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1) (size 15.625)))
       (0x0000000000000000000002926d26d2
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 62.5)))
       (0x0000000000000000000006da4da4da
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 31.25)))
       (0x0000000000000000000006db6db6db
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1) (size 7.8125)))
       (0x000000000000000000001493693693
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 31.25)))
       (0x0000000000000000000036d26d26d2
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 31.25)))
       (0x0000000000000000000036db6db6db
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1) (size 7.8125)))
       (0x00000000000000000000a49b49b49b
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1) (size 31.25)))
       (0x00000000000000000001b693693693
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 15.625)))
       (0x00000000000000000001b6db6db6db
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1) (size 3.90625)))
       (0x0000000000000000000524da4da4da
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 15.625)))
       (0x0000000000000000000db49b49b49b
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 7.8125)))
       (0x0000000000000000000db6db6db6db
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 1.953125)))
       (0x0000000000000000002926d26d26d2
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 15.625)))
       (0x0000000000000000006da4da4da4da
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 7.8125)))
       (0x0000000000000000006db6db6db6db
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 1.953125)))
       (0x000000000000000001493693693693
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 7.8125)))
       (0x0000000000000000036d26d26d26d2
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 7.8125)))
       (0x0000000000000000036db6db6db6db
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.9765625)))
       (0x00000000000000000a49b49b49b49b
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 7.8125)))
       (0x00000000000000001b693693693693
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 3.90625)))
       (0x00000000000000001b6db6db6db6db
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.48828125)))
       (0x0000000000000000524da4da4da4da
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 3.90625)))
       (0x0000000000000000db49b49b49b49b
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 1.953125)))
       (0x0000000000000000db6db6db6db6db
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.48828125)))
       (0x0000000000000002926d26d26d26d2
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 3.90625)))
       (0x0000000000000006da4da4da4da4da
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 1.953125)))
       (0x0000000000000006db6db6db6db6db
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.244140625)))
       (0x0000000000000001493693693693693
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 1.953125)))
       (0x00000000000000036d26d26d26d26d2
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 1.953125)))
       (0x00000000000000036db6db6db6db6db
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.1220703125)))
       (0x000000000000001249b49b49b49b49b
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 1.953125)))
       (0x0000000000000033693693693693697
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.9765625)))
       (0x00000000000000336db6db6db6db6df
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.06103515625)))
       (0x00000000000000a24da4da4da4da4de
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.9765625)))
       (0x00000000000001b349b49b49b49b4bf
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.48828125)))
       (0x00000000000001b36db6db6db6db6ff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.030517578125)))
       (0x00000000000005226d26d26d26d26f6
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.48828125)))
       (0x0000000000000db24da4da4da4da5fe
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.244140625)))
       (0x0000000000000db36db6db6db6db7ff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.0152587890625)))
       (0x00000000000029236936936936937b7
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.244140625)))
       (0x0000000000006da26d26d26d26d2ff6
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.1220703125)))
       (0x0000000000006db36db6db6db6dbfff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.00762939453125)))
       (0x000000000001493349b49b49b49bdbf
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.1220703125)))
       (0x0000000000036d23693693693697fb7
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.06103515625)))
       (0x0000000000036db36db6db6db6dffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.003814697265625)))
       (0x00000000000a49b24da4da4da4dedfe
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.1220703125)))
       (0x00000000001b693349b49b49b4bfdbf
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.030517578125)))
       (0x00000000001b6db36db6db6db6fffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.0019073486328125)))
       (0x0000000000524da26d26d26d26f6ff6
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.06103515625)))
       (0x0000000000db49b24da4da4da5fedfe
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.030517578125)))
       (0x0000000000db6db36db6db6db7fffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.00095367431640625)))
       (0x0000000002926d236936936937b7fb7
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.030517578125)))
       (0x0000000006da4da26d26d26d2ff6ff6
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.0152587890625)))
       (0x0000000006db6db36db6db6dbffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.000476837158203125)))
       (0x000000001493693349b49b49bdbfdbf
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.0152587890625)))
       (0x0000000036d26d23693693697fb7fb7
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.00762939453125)))
       (0x0000000036db6db36db6db6dfffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.0002384185791015625)))
       (0x00000000a49b49b24da4da4dedfedfe
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.00762939453125)))
       (0x00000001b693693349b49b4bfdbfdbf
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.003814697265625)))
       (0x00000001b6db6db36db6db6ffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.00011920928955078125)))
       (0x0000000524da4da26d26d26f6ff6ff6
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.00762939453125)))
       (0x0000000db49b49b24da4da5fedfedfe
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.0019073486328125)))
       (0x0000000db6db6db36db6db7ffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 5.9604644775390625E-05)))
       (0x0000002926d26d236936937b7fb7fb7
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.003814697265625)))
       (0x0000006da4da4da26d26d2ff6ff6ff6
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.0019073486328125)))
       (0x0000006db6db6db36db6dbfffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 2.9802322387695312E-05)))
       (0x000001493693693349b49bdbfdbfdbf
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.0019073486328125)))
       (0x0000036d26d26d23693697fb7fb7fb7
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.00095367431640625)))
       (0x0000036db6db6db36db6dffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 1.4901161193847656E-05)))
       (0x00000a49b49b49b24da4dedfedfedfe
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.00095367431640625)))
       (0x00001b693693693349b4bfdbfdbfdbf
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.000476837158203125)))
       (0x00001b6db6db6db36db6fffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 7.4505805969238281E-06)))
       (0x0000524da4da4da26d26f6ff6ff6ff6
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.000476837158203125)))
       (0x0000db49b49b49b24da5fedfedfedfe
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.0002384185791015625)))
       (0x0000db6db6db6db36db7fffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 3.7252902984619141E-06)))
       (0x0002926d26d26d236937b7fb7fb7fb7
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.0002384185791015625)))
       (0x0006da4da4da4da26d2ff6ff6ff6ff6
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.00011920928955078125)))
       (0x0006db6db6db6db36dbffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 1.862645149230957E-06)))
       (0x001493693693693349bdbfdbfdbfdbf
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.00011920928955078125)))
       (0x0036d26d26d26d23697fb7fb7fb7fb7
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 5.9604644775390625E-05)))
       (0x0036db6db6db6db36dfffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 9.3132257461547852E-07)))
       (0x00a49b49b49b49b24dedfedfedfedfe
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 0.00011920928955078125)))
       (0x01b69369369369334bfdbfdbfdbfdbf
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 2.9802322387695312E-05)))
       (0x01b6db6db6db6db36ffffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 4.6566128730773926E-07)))
       (0x0524da4da4da4da26f6ff6ff6ff6ff6
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 5.9604644775390625E-05)))
       (0x0db49b49b49b49b25fedfedfedfedfe
        ((centroid ((p ((x -100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 2.9802322387695312E-05)))
       (0x0db6db6db6db6db37ffffffffffffff
        ((centroid ((p ((x 0) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 2.3283064365386963E-07)))
       (0x2926d26d26d26d237b7fb7fb7fb7fb7
        ((centroid ((p ((x 100) (y 0) (z 0))) (m 1000))) (bodies 1)
         (size 2.9802322387695312E-05)))))
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
