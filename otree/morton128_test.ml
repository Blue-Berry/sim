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
  print_s [%sexp (shift_right_logical (shift_left one 64) 64 : Int128.t)];
  [%expect {| 0x00000000000000001 |}];
  print_s [%sexp (shift_right_logical (shift_left one 64) 3 : Int128.t)];
  [%expect {| 0x02000000000000000 |}];
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
  let open Unsigned.UInt64 in
  print_s [%sexp (encode zero zero zero : Int128.t)];
  [%expect {| 0x00000000000000000 |}];
  print_s [%sexp (encode one zero zero : Int128.t)];
  [%expect {| 0x00000000000000001 |}];
  print_s [%sexp (encode one one one : Int128.t)];
  [%expect {| 0x00000000000000007 |}]
;;

let%expect_test "float conversion" =
  let open Unsigned.UInt64 in
  printf
    "%s => %s\n"
    (Float.int_pow 2.0 63 |> Float.to_string_12)
    (uint64_of_float (Float.int_pow 2.0 63) |> to_string);
  [%expect {| 9.22337203685e+18 => 9223372036854775808 |}]
;;

let%expect_test "decode" =
  let open Unsigned in
  let print_point (x, y, z) =
    let open UInt64 in
    Printf.printf
      "x: %s; y: %s; z: %s;"
      (to_hexstring x)
      (to_hexstring y)
      (to_hexstring z)
  in
  decode Int128.one |> print_point;
  [%expect {| x: 1; y: 0; z: 0; |}];
  decode (Int128.of_hex "0x000000000000000000000000000000") |> print_point;
  [%expect {| x: 0; y: 0; z: 0; |}];
  decode (Int128.of_hex "0x000000000000000000000000000001") |> print_point;
  [%expect {| x: 1; y: 0; z: 0; |}];
  decode (Int128.of_hex "0x000000000000000000000000000007") |> print_point;
  [%expect {| x: 1; y: 1; z: 1; |}];
  decode (Int128.of_hex "0x000000000000000fffffffffffffff") |> print_point;
  [%expect {| x: fffff; y: fffff; z: fffff; |}];
  decode Int128.(shift_left one 63) |> print_point;
  [%expect {| x: 200000; y: 0; z: 0; |}];
  decode Int128.(shift_left one 64) |> print_point;
  [%expect {| x: 0; y: 200000; z: 0; |}];
  decode Int128.(shift_left one 65) |> print_point;
  [%expect {| x: 0; y: 0; z: 200000; |}];
  decode Int128.(shift_left one 120) |> print_point;
  [%expect {| x: 10000000000; y: 0; z: 0; |}];
  decode Int128.(shift_left one ((42 * 3) - 1)) |> print_point;
  [%expect {| x: 0; y: 0; z: 20000000000; |}];
  decode (Int128.of_hex "0xffffffffffffffffffffffffffffff") |> print_point;
  [%expect {| x: ffffffffff; y: ffffffffff; z: ffffffffff; |}]
;;

let%expect_test "morton point" =
  let open Unsigned in
  let print_point (x, y, z) =
    let open UInt64 in
    Printf.printf
      "x: %s; y: %s; z: %s;"
      (to_hexstring x)
      (to_hexstring y)
      (to_hexstring z)
  in
  let point1 = Physics.point (-100.) (-100.) (-100.) in
  let point2 = Physics.point 100. 100. 100. in
  let point3 = Physics.point 0. 0. 100. in
  let point4 = Physics.point 0. 100. 0. in
  let point5 = Physics.point 100. 0. 0. in
  let bb =
    Utils.Bb.
      { x_min = -100.
      ; x_max = 100.
      ; y_min = -100.
      ; y_max = 100.
      ; z_min = -100.
      ; z_max = 100.
      }
  in
  point_to_grid point1 bb |> print_point;
  [%expect {| x: 0; y: 0; z: 0; |}];
  point_to_grid point2 bb |> print_point;
  [%expect {| x: 3ffffffffff; y: 3ffffffffff; z: 3ffffffffff; |}];
  point_to_grid point3 bb |> print_point;
  [%expect {| x: 1ffffffffff; y: 1ffffffffff; z: 3ffffffffff; |}];
  point_to_grid point4 bb |> print_point;
  [%expect {| x: 1ffffffffff; y: 3ffffffffff; z: 1ffffffffff; |}];
  point_to_grid point5 bb |> print_point;
  [%expect {| x: 3ffffffffff; y: 1ffffffffff; z: 1ffffffffff; |}];
  print_s [%sexp (encode_point point1 bb : Int128.t)];
  [%expect {| 0x00000000000000000 |}];
  print_s [%sexp (encode_point point2 bb : Int128.t)];
  [%expect {| 0x3fffffffffffffffffffffffffffffff |}];
  print_s [%sexp (encode_point point3 bb : Int128.t)];
  [%expect {| 0x27ffffffffffffffffffffffffffffff |}];
  print_s [%sexp (encode_point point4 bb : Int128.t)];
  [%expect {| 0x17ffffffffffffffffffffffffffffff |}];
  print_s [%sexp (encode_point point5 bb : Int128.t)];
  [%expect {| 0xfffffffffffffffffffffffffffffff |}]
;;

let%expect_test "levels and parents" =
  print_s
    [%sexp (parent_morton Int128.(of_hex "0x9249249249249249249249249249249") : Int128.t)];
  [%expect {| 0x1249249249249249249249249249249 |}];
  print_s [%sexp (parent_morton Int128.(of_hex "0x0") : Int128.t)];
  [%expect {| 0x00000000000000000 |}];
  print_s [%sexp (parent_morton Int128.(of_hex "0xf") : Int128.t)];
  [%expect {| 0x00000000000000001 |}];
  print_s
    [%sexp
      (morton_at_level Int128.(of_hex "0x9249249249249249249249249249249") 0 : Int128.t)];
  [%expect {| 0x9249249249249249249249249249249 |}];
  print_s
    [%sexp
      (morton_at_level Int128.(of_hex "0x9249249249249249249249249249249") 40 : Int128.t)];
  [%expect {| 0x00000000000000009 |}]
;;
