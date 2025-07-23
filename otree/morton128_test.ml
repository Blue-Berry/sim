open! Core
open! Utils.Morton128

(* type t = Utils.Morton126.t = { high : int; low : int } *)
let%expect_test "Int128" =
  let open Int128 in
  print_s [%sexp (zero : Int128.t)];
  [%expect {| 0x00000000000000000 |}];
  print_s [%sexp (one : Int128.t)];
  [%expect {| 0x00000000000000001 |}];
  print_s [%sexp (of_int 16 : Int128.t)];
  [%expect {| 0x00000000000000010 |}];
  print_s [%sexp (shift_left one 1 : Int128.t)];
  [%expect {| 0x00000000000000002 |}];
  print_s [%sexp (shift_left one 63 : Int128.t)];
  [%expect {| 0x08000000000000000 |}];
  print_s [%sexp (shift_left one 64 : Int128.t)];
  [%expect {| 0x10000000000000000 |}];
  print_s [%sexp (shift_left one 120 : Int128.t)];
  [%expect {| 0x1000000000000000000000000000000 |}];
  let { low; high } = shift_left one 120 in
  printf "Low: %d; High: %d;" (Unsigned.UInt64.to_int low) (Unsigned.UInt64.to_int high);
  [%expect {| Low: 0; High: 72057594037927936; |}];
  print_s [%sexp (shift_left one 127 : Int128.t)];
  [%expect {| 0x80000000000000000000000000000000 |}];
  print_s [%sexp (shift_right_logical (shift_left one 63) 63 : Int128.t)];
  [%expect {| 0x00000000000000001 |}];
  print_s [%sexp (shift_right_logical (shift_left one 127) 127 : Int128.t)];
  [%expect {| 0x00000000000000001 |}];
  print_s [%sexp (compare zero one : int)];
  [%expect {| -1 |}];
  print_s [%sexp (test_bit (shift_left one 120) 120 : bool)];
  [%expect {| true |}];
  print_s [%sexp (test_bit one 0 : bool)];
  [%expect {| true |}];
  print_s [%sexp (test_bit (shift_left one 1) 1 : bool)];
  [%expect {| true |}];
  print_s [%sexp (test_bit (shift_left one 63) 63 : bool)];
  [%expect {| true |}];
  print_s [%sexp (test_bit (shift_left one 64) 64 : bool)];
  [%expect {| true |}];
  print_s [%sexp (test_bit (shift_left one 120) 120 : bool)];
  [%expect {| true |}];
  print_s [%sexp (add (shift_left one 63) (shift_left one 63) : Int128.t)];
  [%expect {| 0x10000000000000000 |}];
  print_s [%sexp (of_hex "0x80000000000000000000000000000000" : Int128.t)];
  [%expect {| 0x80000000000000000000000000000000 |}];
  print_s [%sexp (sexp_of_t one : Sexp.t)];
  [%expect {| 0x00000000000000001 |}];
  print_s [%sexp (t_of_sexp (Sexp.of_string "0x00000000000000001") : Int128.t)];
  [%expect {| 0x00000000000000001 |}];
  print_s [%sexp (popcount (of_hex "0xff") : int)];
  [%expect {| 8 |}]
;;

(* TODO:  val encode : int -> int -> int -> Int128.t *)
(* TODO:  val decode : Int128.t -> int * int * int *)
(* TODO:  val point_to_grid : Physics.point -> Utils.Bb.t -> int * int * int *)
(* TODO:  val encode_point : Physics.point -> Utils.Bb.t -> Int128.t *)
(* TODO:  val parent_morton : Int128.t -> Int128.t *)
(* TODO:  val morton_at_level : Int128.t -> int -> Int128.t *)

let%expect_test "encode" =
  print_s [%sexp (encode 0 0 0 : Int128.t)];
  [%expect {| 0x00000000000000000 |}];
  print_s [%sexp (encode 1 0 0 : Int128.t)];
  [%expect {| 0x00000000000000001 |}]
;;

(* let%expect_test "decode" = *)
(*   let x, y, z = decode Int128.one in *)
(*   Printf.printf "x: %d; y: %d; z: %d;" x y z; *)
(*   [%expect {| x: 2199023255552; y: 0; z: 0; |}]; *)
(*   let x, y, z = decode (Int128.of_hex "0x000000000000000000000000000000") in *)
(*   Printf.printf "x: %d; y: %d; z: %d;" x y z; *)
(*   [%expect.unreachable]; *)
(*   let x, y, z = decode (Int128.of_hex "0x000000000000000000000000000001") in *)
(*   Printf.printf "x: %d; y: %d; z: %d;" x y z; *)
(* ; *)
