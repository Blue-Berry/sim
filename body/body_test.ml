let%expect_test _ =
  print_endline "Hello, world!";
  [%expect {| Hello, world! |}]
;;

open! Core
open Body

let v = Physics.vec

let%expect_test "displace" =
  let u = v 1. 1. 1. in
  Physics.displace u (v 1. 1. 1.);
  u |> Physics.print;
  [%expect {| x: 2.0000e+00 y: 2.0000e+00 z: 2.0000e+00 |}]
;;

let%expect_test "mag_sq" =
  Physics.mag_sq (v 1. 1. 1.) |> Float.to_string |> print_endline;
  [%expect {| 3. |}]
;;

let%expect_test "mag" =
  Physics.mag (v 1. 0. 0.) |> Float.to_string |> print_endline;
  [%expect {| 1. |}]
;;

let%expect_test "unit_vec" =
  let open Physics in
  vec_norm (v 3. 3. 3.) |> Physics.print;
  [%expect {| x: 5.7735e-01 y: 5.7735e-01 z: 5.7735e-01 |}]
;;

let%expect_test "unit_vec" =
  let open Physics in
  vec_norm (v 3. 3. 3.) |> mag |> Float.to_string |> print_endline;
  [%expect {| 1. |}]
;;

let%expect_test "-->" =
  let open Physics in
  let v1 = v 0. 0. 1.
  and v2 = v 1. 1. 1. in
  v1 --> v2 |> print;
  [%expect {| x: 1.0000e+00 y: 1.0000e+00 z: 0.0000e+00 |}]
;;

let%expect_test "close_enough" =
  let open Physics in
  close_enough (v 1. 1. 1.00000001) (v 1. 1. 1.) |> Bool.to_string |> print_endline;
  [%expect {| true |}]
;;

let%expect_test "acc_on" =
  let open Physics in
  let open Float in
  acc_on (v 0. 0. 0.) (v 1. 1. 1.) (1. / g) |> print;
  [%expect {| x: 1.9245e-01 y: 1.9245e-01 z: 1.9245e-01 |}];
  acc_on (v 0. 0. 0.) (v 1. 0. 0.) (1. / g) |> mag |> Float.to_string |> print_endline;
  [%expect {| 1. |}]
;;

let%expect_test "new_position" =
  let open Physics in
  let u = v 0. 0. 0. in
  new_position ~pos:u ~t:1.0 ~v:(v 1. 1. 1.) ~a:(v 0. 0. 0.);
  u |> print;
  [%expect {| x: 1.0000e+00 y: 1.0000e+00 z: 1.0000e+00 |}]
;;

let%expect_test "new_velocity" =
  let open Physics in
  let u = v 0. 0. 0. in
  new_velocity ~v:u ~a:(v 1. 1. 1.) ~t:1.0;
  u |> print;
  [%expect {| x: 1.0000e+00 y: 1.0000e+00 z: 1.0000e+00 |}]
;;

let%expect_test "step" =
  let b : Body.t = Body.{ mass = 1.; pos = v 0. 0. 0.; vel = v 1. 0. 0. } in
  step b (v 0. 0. 0.) 1.0;
  b |> print;
  [%expect
    {|
    M: 1.00
    P: x: 1.0000e+00 y: 0.0000e+00 z: 0.0000e+00
    V: x: 1.0000e+00 y: 0.0000e+00 z: 0.0000e+00
    |}]
;;

let%expect_test "accelerate_body" =
  let b : Body.t = Body.{ mass = 1.0E12; pos = v 0. 0. 0.; vel = v 1. 0. 0. } in
  accelerate_body [ b ] (v 1. 1. 1.) |> Physics.print;
  [%expect {| x: -1.2845e+01 y: -1.2845e+01 z: -1.2845e+01 |}]
;;

let%expect_test "accelerations" =
  let b1 : Body.t = Body.{ mass = 100000.0E12; pos = v 0. 0. 0.; vel = v 1. 0. 0. } in
  let b2 : Body.t = Body.{ mass = 100000.0E12; pos = v 1. 1. 1.; vel = v 1. 0. 0. } in
  let b3 : Body.t = Body.{ mass = 100000.0E12; pos = v 2. 2. 2.; vel = v 1. 0. 0. } in
  accelerations [ b1; b2; b3 ] |> List.iter ~f:Physics.print;
  [%expect
    {|
    x: 1.6056e+06 y: 1.6056e+06 z: 1.6056e+06
    x: 0.0000e+00 y: 0.0000e+00 z: 0.0000e+00
    x: -1.6056e+06 y: -1.6056e+06 z: -1.6056e+06
    |}]
;;

let%expect_test "step_bodies" =
  let bs : t list =
    [ { mass = 100000.0E10; pos = v 0. 0. 0.; vel = v 0. 0. 0. }
    ; { mass = 100000.0E10; pos = v 1. 1. 1.; vel = v 0. 0. 0. }
    ; { mass = 100000.0E10; pos = v 2. 2. 2.; vel = v 0. 0. 0. }
    ]
  in
  step_bodies bs 1.;
  bs |> List.iter ~f:print;
  [%expect
    {|
    M: 1000000000000000.00
    P: x: 8.0279e+03 y: 8.0279e+03 z: 8.0279e+03
    V: x: 1.6056e+04 y: 1.6056e+04 z: 1.6056e+04
    M: 1000000000000000.00
    P: x: 1.0000e+00 y: 1.0000e+00 z: 1.0000e+00
    V: x: 0.0000e+00 y: 0.0000e+00 z: 0.0000e+00
    M: 1000000000000000.00
    P: x: -8.0259e+03 y: -8.0259e+03 z: -8.0259e+03
    V: x: -1.6056e+04 y: -1.6056e+04 z: -1.6056e+04
    |}]
;;

let%expect_test "orthogonal_vector" =
  let open Utils in
  orthogonal_vector (v 1. 1. 1.) |> Physics.print;
  [%expect {| x: -8.1213e-01 y: 3.3305e-01 z: 4.7908e-01 |}];
  orthogonal_vector (v 1. 1. 1.)
  |> Owl.Mat.transpose
  |> Owl.Mat.dot (v 1. 1. 1.)
  |> Owl.Mat.trace
  |> Float.round ~dir:`Down
  |> Float.to_string
  |> print_endline;
  [%expect {| 0. |}]
;;
