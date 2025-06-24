open! Core
open! Otree

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
  [%expect {| ((x_min -1) (x_max 0) (y_min -1) (y_max 0) (z_min -1) (z_max 0)) |}];
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
  [%expect {| ((x_min -1) (x_max 3) (y_min -1) (y_max 3) (z_min -1) (z_max 3)) |}];
