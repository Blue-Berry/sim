open Body
open! Core

type t =
  { p : Physics.point
  ; m : float
  }

let pp fmt (c : t) = Format.fprintf fmt "P: %a M: %.2f" Physics.pp c.p c.m
let empty = { p = Physics.zero; m = 0. }

let add (c1 : t) (c2 : t) : t =
  let open Owl.Mat in
  let m : float = c1.m +. c2.m in
  let p : Physics.point = ((c1.m $* c1.p) + (c2.m $* c2.p)) /$ m in
  { p; m }
;;
