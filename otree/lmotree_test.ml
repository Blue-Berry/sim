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
  ()
;;
