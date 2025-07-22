open! Core
open! Utils.Morton126

(* TODO:  val encode : int -> int -> int -> Int126.t *)
(* TODO:  val decode : Int126.t -> int * int * int *)
(* TODO:  val point_to_grid : Physics.point -> Utils.Bb.t -> int * int * int *)
(* TODO:  val encode_point : Physics.point -> Utils.Bb.t -> Int126.t *)
(* TODO:  val parent_morton : Int126.t -> Int126.t *)
(* TODO:  val morton_at_level : Int126.t -> int -> Int126.t *)

let%expect_test "encode" =
  print_s [%sexp (encode 0 0 0 : Int126.t)];
  [%expect {| 0x000000000000000000000000000000 |}];
  print_s [%sexp (encode 1 0 0 : Int126.t)];
  [%expect {| 0x000000000000000000000000000001 |}];
  print_s [%sexp (encode Int126.max_63_bit 0 0 : Int126.t)];
  [%expect {| 0x12492492492492491249249249249249 |}]
;;

let%expect_test "decode" =
  let x, y, z = decode (Int126.of_hex "0x000000000000000000000000000000") in
  Printf.printf "x: %d; y: %d; z: %d;" x y z;
  [%expect {| x: 0; y: 0; z: 0; |}];
  let x, y, z = decode (Int126.of_hex "0x000000000000000000000000000001") in
  Printf.printf "x: %d; y: %d; z: %d;" x y z;
  [%expect {| x: 2199023255552; y: 0; z: 0; |}];
  let x, y, z = decode (Int126.of_hex "0x12492492492492491249249249249249") in
  Printf.printf "x: %d; y: %d; z: %d;" x y z;
  [%expect {| x: 0; y: 4398029733888; z: 14680060; |}]
;;
