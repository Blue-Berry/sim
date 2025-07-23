open! Core
open! Utils.Morton128

(* type t = Utils.Morton126.t = { high : int; low : int } *)
let%expect_test "Int128" =
  let open Int128 in
  print_s [%sexp (zero : Int128.t)];
  [%expect {| 0x00000000000000000 |}];
  print_s [%sexp (one : Int128.t)];
  [%expect {| 0x00000000000000001 |}];
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
  [%expect {| 0x80000000000000000000000000000000 |}]
;;

(* val shift_left : t -> int -> t *)
(* val shift_right_logical : t -> int -> t *)
(* val logand : t -> t -> t *)
(* val logor : t -> t -> t *)
(* val logxor : t -> t -> t *)
(* val compare : t -> t -> int *)
(* val equal : t -> t -> bool *)
(* val test_bit : t -> int -> bool *)
(* val to_hex : t -> string *)
(* val of_hex : string -> t *)
(* val to_string : t -> string *)
(* val sexp_of_t : t -> Sexplib0.Sexp.t *)
(* val t_of_sexp : Sexplib0.Sexp.t -> t *)

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

let%expect_test "decode" =
  let x, y, z = decode Int128.one in
  Printf.printf "x: %d; y: %d; z: %d;" x y z;
  [%expect {| x: 2199023255552; y: 0; z: 0; |}];
  let x, y, z = decode (Int128.of_hex "0x000000000000000000000000000000") in
  Printf.printf "x: %d; y: %d; z: %d;" x y z;
  [%expect.unreachable];
  let x, y, z = decode (Int128.of_hex "0x000000000000000000000000000001") in
  Printf.printf "x: %d; y: %d; z: %d;" x y z;
  [%expect.unreachable];
  let x, y, z = decode (Int128.of_hex "0x12492492492492491249249249249249") in
  Printf.printf "x: %d; y: %d; z: %d;" x y z;
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  (Failure TODO)
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Morton128_test.(fun) in file "otree/morton128_test.ml", line 58, characters 23-73
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28
  |}]
;;
