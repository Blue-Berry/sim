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
  [%expect {| x: 2.0000 y: 2.0000 z: 2.0000 |}]
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
  [%expect {| x: 0.5774 y: 0.5774 z: 0.5774 |}]
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
  [%expect {| x: 1.0000 y: 1.0000 z: 0.0000 |}]
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
  [%expect {| x: 0.1925 y: 0.1925 z: 0.1925 |}];
  acc_on (v 0. 0. 0.) (v 1. 0. 0.) (1. / g) |> mag |> Float.to_string |> print_endline;
  [%expect {| 1. |}]
;;

let%expect_test "new_position" =
  let open Physics in
  let u = v 0. 0. 0. in
  new_position ~pos:u ~t:1.0 ~v:(v 1. 1. 1.) ~a:(v 0. 0. 0.);
  u |> print;
  [%expect {| x: 1.0000 y: 1.0000 z: 1.0000 |}]
;;

let%expect_test "new_velocity" =
  let open Physics in
  let u = v 0. 0. 0. in
  new_velocity ~v:u ~a:(v 1. 1. 1.) ~t:1.0;
  u |> print;
  [%expect {| x: 1.0000 y: 1.0000 z: 1.0000 |}]
;;

let%expect_test "step" =
  let b : Body.t = Body.{ mass = 1.; pos = v 0. 0. 0.; vel = v 1. 0. 0. } in
  step b (v 0. 0. 0.) 1.0;
  b |> print;
  [%expect
    {|
    M: 1.00
    P: x: 1.0000 y: 0.0000 z: 0.0000
    V: x: 1.0000 y: 0.0000 z: 0.0000
    |}]
;;

let%expect_test "accelerate_body" =
  let b : Body.t = Body.{ mass = 1.0E12; pos = v 0. 0. 0.; vel = v 1. 0. 0. } in
  accelerate_body [ b ] (v 1. 1. 1.) |> Physics.print;
  [%expect {| x: -12.8447 y: -12.8447 z: -12.8447 |}]
;;

let%expect_test "accelerations" =
  let b1 : Body.t = Body.{ mass = 100000.0E12; pos = v 0. 0. 0.; vel = v 1. 0. 0. } in
  let b2 : Body.t = Body.{ mass = 100000.0E12; pos = v 1. 1. 1.; vel = v 1. 0. 0. } in
  let b3 : Body.t = Body.{ mass = 100000.0E12; pos = v 2. 2. 2.; vel = v 1. 0. 0. } in
  accelerations [ b1; b2; b3 ] |> List.iter ~f:Physics.print;
  [%expect
    {|
    x: 1605582.2311 y: 1605582.2311 z: 1605582.2311
    x: 0.0000 y: 0.0000 z: 0.0000
    x: -1605582.2311 y: -1605582.2311 z: -1605582.2311
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
    P: x: 8027.9112 y: 8027.9112 z: 8027.9112
    V: x: 16055.8223 y: 16055.8223 z: 16055.8223
    M: 1000000000000000.00
    P: x: 1.0000 y: 1.0000 z: 1.0000
    V: x: 0.0000 y: 0.0000 z: 0.0000
    M: 1000000000000000.00
    P: x: -8025.9112 y: -8025.9112 z: -8025.9112
    V: x: -16055.8223 y: -16055.8223 z: -16055.8223
    |}]
;;

let%expect_test "orthogonal_vector" =
  let open Utils in
  orthogonal_vector (v 1. 1. 1.) |> Physics.print;
  [%expect {| x: -0.8121 y: 0.3331 z: 0.4791 |}];
  orthogonal_vector (v 1. 1. 1.)
  |> Owl.Mat.transpose
  |> Owl.Mat.dot (v 1. 1. 1.)
  |> Owl.Mat.trace
  |> Float.round ~dir:`Down
  |> Float.to_string
  |> print_endline;
  [%expect {| 0. |}]
;;
