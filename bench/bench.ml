open! Core_bench
open! Otree
open! Core

let bench () =
  let bounds =
    Bb.
      { x_min = -100.
      ; x_max = 100.
      ; y_min = -100.
      ; y_max = 100.
      ; z_min = -100.
      ; z_max = 100.
      }
  in
  let open Physics in
  (* Create multiple bodies for performance test *)
  let create_bodies n =
    List.init n ~f:(fun i ->
      let angle = Float.(2. * pi * of_int i / of_int n) in
      let radius = 50. in
      Body.
        { pos =
            point
              (radius *. Float.cos angle)
              (radius *. Float.sin angle)
              (Float.of_int i *. 2.)
        ; mass = 1000.0 +. Float.of_int i
        ; vel = vec 0. 0. 0.
        })
  in
  let bodies = create_bodies 5000 in
  for _i = 0 to 1000 do
    let tree = Otree.create_tree ~capacity:10000 ~theta:0.1 in
    let centroids = List.map bodies ~f:(fun body -> C.{ m = body.mass; p = body.pos }) in
    List.iter centroids ~f:(fun c -> Otree.insert_body tree c bounds);
    let forces = Otree.calculate_all_forces tree bodies bounds in
    update_simulation forces 0.1
  done;
  List.iter bodies ~f:Body.print
;;

let () =
  Command_unix.run (Bench.make_command [ Bench.Test.create ~name:"Base Octree" bench ])
;;
