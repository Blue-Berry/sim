open! Core
open! Owl.Maths

type point =
  { x : float
  ; y : float
  ; z : float
  }

type centroid =
  { p : point
  ; m : float
  }

type vec =
  { x : float
  ; y : float
  ; z : float
  }

let displace ({ x; y; z } : point) ({ x = dx; y = dy; z = dz } : vec) : point =
  { x = x +. dx; y = y +. dy; z = z +. dz }
;;

let mag_sq ({ x; y; z } : vec) : float = sqr x +. sqr y +. sqr z
let mag v = sqrt (mag_sq v)
let ( *$ ) c ({ x; y; z } : vec) = { x = x *. c; y = y *. c; z = z *. c }
let unit_vec (v : vec) : vec = 1.0 /. mag v *$ v [@@inline]

let ( --> ) ({ x = x1; y = y1; z = z1 } : point) ({ x = x2; y = y2; z = z2 } : point)
  : vec
  =
  { x = x2 -. x1; y = y2 -. y1; z = z2 -. z1 }
;;

let ( ++ ) ({ x = x1; y = y1; z = z1 } : vec) ({ x = x2; y = y2; z = z2 } : vec) : vec =
  { x = x1 +. x2; y = y1 +. y2; z = z1 +. z2 }
;;

let zero : vec = { x = 0.0; y = 0.0; z = 0.0 }

let close_enough ({ x = x1; y = y1; z = z1 } : point) ({ x = x2; y = y2; z = z2 } : point)
  =
  Float.(mag_sq { x = x1 -. x2; y = y1 -. y2; z = z1 -. z2 } <. 1.0e-5)
;;

let g = 6.67428e-11 (* N (m/kg)^2 *)

let acc_on (p : point) (c : centroid) : vec =
  match close_enough p c.p with
  | true -> zero
  | false ->
    let d = p --> c.p in
    let r = mag_sq d in
    let aMag = g *. c.m /. r in
    let aDir = unit_vec d in
    aMag *$ aDir
;;
