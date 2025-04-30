let%expect_test _ =
  print_endline "Hello, world!";
  [%expect {| Hello, world! |}]
;;

open! Core
open Body

let%expect_test _ =
  Physics.displace { x = 1.; y = 1.; z = 1. } { x = 1.; y = 1.; z = 1. }
  |> Physics.sexp_of_point
  |> Sexp.to_string_hum
  |> print_endline;
  [%expect {| ((x 2) (y 2) (z 2)) |}]
;;

let%expect_test _ =
  Physics.mag_sq { x = 1.; y = 1.; z = 1. } |> Float.to_string |> print_endline;
  [%expect {| 3. |}]
;;

let%expect_test _ =
  Physics.mag { x = 1.; y = 0.; z = 0. } |> Float.to_string |> print_endline;
  [%expect {| 1. |}]
;;

let%expect_test _ =
  let open Physics in
  Physics.( *$ ) 2.0 { x = 1.; y = 1.; z = 1. }
  |> sexp_of_vec
  |> Sexp.to_string_hum
  |> print_endline;
  [%expect {| ((x 2) (y 2) (z 2)) |}]
;;

let%expect_test _ =
  let open Physics in
  unit_vec { x = 3.; y = 3.; z = 3. }
  |> sexp_of_vec
  |> Sexp.to_string_hum
  |> print_endline;
  [%expect
    {| ((x 0.57735026918962573) (y 0.57735026918962573) (z 0.57735026918962573)) |}]
;;

let%expect_test _ =
  let open Physics in
  unit_vec { x = 3.; y = 3.; z = 3. } |> mag |> Float.to_string |> print_endline;
  [%expect {| 1. |}]
;;

let%expect_test _ =
  let open Physics in
  { x = 0.; y = 0.; z = 1. } --> { x = 1.; y = 1.; z = 1. }
  |> sexp_of_vec
  |> Sexp.to_string_hum
  |> print_endline;
  [%expect {| ((x 1) (y 1) (z 0)) |}]
;;

let%expect_test _ =
  let open Physics in
  { x = 0.; y = 0.; z = 1. } +$ { x = 1.; y = 1.; z = 1. }
  |> sexp_of_vec
  |> Sexp.to_string_hum
  |> print_endline;
  [%expect {| ((x 1) (y 1) (z 2)) |}]
;;

let%expect_test _ =
  let open Physics in
  close_enough { x = 1.; y = 1.; z = 1.00000001 } { x = 1.; y = 1.; z = 1. }
  |> Bool.to_string
  |> print_endline;
  [%expect {| true |}]
;;

let%expect_test _ =
  let open Physics in
  let open Float in
  acc_on { x = 0.; y = 0.; z = 0. } { x = 1.; y = 1.; z = 1. } (1. / g)
  |> sexp_of_vec
  |> Sexp.to_string_hum
  |> print_endline;
  [%expect
    {| ((x 0.19245008972987526) (y 0.19245008972987526) (z 0.19245008972987526)) |}];
  acc_on { x = 0.; y = 0.; z = 0. } { x = 1.; y = 0.; z = 0. } (1. / g)
  |> mag
  |> Float.to_string
  |> print_endline;
  [%expect {| 1. |}]
;;

let%expect_test _ =
  let open Physics in
  new_position
    { x = 0.; y = 0.; z = 0. }
    1.0
    { x = 1.; y = 1.; z = 1. }
    { x = 0.; y = 0.; z = 0. }
  |> sexp_of_point
  |> Sexp.to_string_hum
  |> print_endline;
  [%expect {| ((x 1) (y 1) (z 1)) |}]
;;

let%expect_test _ =
  let open Physics in
  new_velocity { x = 0.; y = 0.; z = 0. } { x = 1.; y = 1.; z = 1. } 1.0
  |> sexp_of_vec
  |> Sexp.to_string_hum
  |> print_endline;
  [%expect {| ((x 1) (y 1) (z 1)) |}]
;;

let%expect_test _ =
  let b : Body.t =
    Body.{ mass = 1.; pos = { x = 0.; y = 0.; z = 0. }; vel = { x = 1.; y = 0.; z = 0. } }
  in
  step b { x = 0.; y = 0.; z = 0. } 1.0;
  b |> sexp_of_t |> Sexp.to_string_hum |> print_endline;
  [%expect {| ((mass 1) (pos ((x 1) (y 0) (z 0))) (vel ((x 1) (y 0) (z 0)))) |}]
;;

let%expect_test _ =
  let b : Body.t =
    Body.{ mass = 1.; pos = { x = 0.; y = 0.; z = 0. }; vel = { x = 1.; y = 0.; z = 0. } }
  in
  accelerate_body [ b ] { x = 1.; y = 1.; z = 1. }
  |> Physics.sexp_of_vec
  |> Sexp.to_string_hum
  |> print_endline;
  [%expect
    {|
    ((x -1.2844657848823119E-11) (y -1.2844657848823119E-11)
     (z -1.2844657848823119E-11))
    |}]
;;

let%expect_test _ =
  let b1 : Body.t =
    Body.
      { mass = 100000.
      ; pos = { x = 0.; y = 0.; z = 0. }
      ; vel = { x = 1.; y = 0.; z = 0. }
      }
  in
  let b2 : Body.t =
    Body.
      { mass = 100000.
      ; pos = { x = 1.; y = 1.; z = 1. }
      ; vel = { x = 1.; y = 0.; z = 0. }
      }
  in
  let b3 : Body.t =
    Body.
      { mass = 100000.
      ; pos = { x = 2.; y = 2.; z = 2. }
      ; vel = { x = 1.; y = 0.; z = 0. }
      }
  in
  accelerations [ b1; b2; b3 ]
  |> sexp_of_list Physics.sexp_of_vec
  |> Sexp.to_string_hum
  |> print_endline;
  [%expect
    {|
    (((x 1.6055822311028897E-06) (y 1.6055822311028897E-06)
      (z 1.6055822311028897E-06))
     ((x 0) (y 0) (z 0))
     ((x -1.6055822311028897E-06) (y -1.6055822311028897E-06)
      (z -1.6055822311028897E-06)))
    |}]
;;

let%expect_test _ =
  let bs : t list =
    [ { mass = 100000.
      ; pos = { x = 0.; y = 0.; z = 0. }
      ; vel = { x = 0.; y = 0.; z = 0. }
      }
    ; { mass = 100000.
      ; pos = { x = 1.; y = 1.; z = 1. }
      ; vel = { x = 0.; y = 0.; z = 0. }
      }
    ; { mass = 100000.
      ; pos = { x = 2.; y = 2.; z = 2. }
      ; vel = { x = 0.; y = 0.; z = 0. }
      }
    ]
  in
  step_bodies bs 1.;
  bs
  |> sexp_of_list sexp_of_t
  |> Sexp.to_string_hum
  |> print_endline;
  [%expect
    {|
    (((mass 100000)
      (pos
       ((x 8.0279111555144484E-07) (y 8.0279111555144484E-07)
        (z 8.0279111555144484E-07)))
      (vel
       ((x 1.6055822311028897E-06) (y 1.6055822311028897E-06)
        (z 1.6055822311028897E-06))))
     ((mass 100000) (pos ((x 1) (y 1) (z 1))) (vel ((x 0) (y 0) (z 0))))
     ((mass 100000)
      (pos
       ((x 1.9999991972088844) (y 1.9999991972088844) (z 1.9999991972088844)))
      (vel
       ((x -1.6055822311028897E-06) (y -1.6055822311028897E-06)
        (z -1.6055822311028897E-06)))))
    |}]
;;
