open! Core
module Physics = Body.Physics

module Int126 = struct
  (* Use two native OCaml ints (63 bits each) for better performance *)
  type t =
    { high : int (* Most significant 63 bits *)
    ; low : int (* Least significant 63 bits *)
    }
  [@@deriving sexp]

  let zero = { high = 0; low = 0 }
  let one = { high = 0; low = 1 }
  let max_63_bit = (1 lsl 62) - 1 (* 2^62 - 1, avoiding sign bit *)
  let mask_63 = max_63_bit
  let of_int i = { high = 0; low = i land mask_63 }

  let shift_left t bits =
    if bits >= 63
    then { high = (t.low lsl (bits - 63)) land mask_63; low = 0 }
    else if bits = 0
    then t
    else (
      let high' = (t.high lsl bits) lor (t.low lsr (63 - bits)) land mask_63 in
      let low' = (t.low lsl bits) land mask_63 in
      { high = high'; low = low' })
  ;;

  let shift_right_logical t bits =
    if bits >= 63
    then { high = 0; low = t.high lsr (bits - 63) }
    else if bits = 0
    then t
    else (
      let low' =
        (t.low lsr bits) lor ((t.high land ((1 lsl bits) - 1)) lsl (63 - bits))
      in
      let high' = t.high lsr bits in
      { high = high'; low = low' })
  ;;

  let logand t1 t2 = { high = t1.high land t2.high; low = t1.low land t2.low }
  let logor t1 t2 = { high = t1.high lor t2.high; low = t1.low lor t2.low }
  let logxor t1 t2 = { high = t1.high lxor t2.high; low = t1.low lxor t2.low }

  let compare t1 t2 =
    let high_cmp = Int.compare t1.high t2.high in
    if high_cmp = 0 then Int.compare t1.low t2.low else high_cmp
  ;;

  let equal t1 t2 = t1.high = t2.high && t1.low = t2.low

  let test_bit t bit_pos =
    if bit_pos >= 63
    then (t.high lsr (bit_pos - 63)) land 1 = 1
    else (t.low lsr bit_pos) land 1 = 1
  ;;

  let to_hex t = Printf.sprintf "%015x%015x" t.high t.low

  let to_string t =
    if t.high = 0 then string_of_int t.low else Printf.sprintf "%d%015d" t.high t.low
  ;;
end

type t = Int126.t

let precision = 126 / 3

let encode x y z =
  let rec interleave acc x y z depth =
    if depth = 0
    then acc
    else (
      (* Extract bits using fast native operations *)
      let mask = 1 lsl (depth - 1) in
      let bit_x = x land mask in
      let bit_y = y land mask in
      let bit_z = z land mask in
      (* Pack bits efficiently *)
      let acc_shifted = Int126.shift_left acc 3 in
      let bits_packed =
        (if bit_z <> 0 then 4 else 0)
        lor (if bit_y <> 0 then 2 else 0)
        lor if bit_x <> 0 then 1 else 0
      in
      let combined = Int126.logor acc_shifted (Int126.of_int bits_packed) in
      interleave combined x y z (depth - 1))
  in
  interleave Int126.zero x y z precision
;;

let decode morton =
  let rec extract_bits morton depth x_acc y_acc z_acc =
    if depth = 0
    then x_acc, y_acc, z_acc
    else (
      let bit_x = if Int126.test_bit morton 0 then 1 lsl (depth - 1) else 0 in
      let bit_y = if Int126.test_bit morton 1 then 1 lsl (depth - 1) else 0 in
      let bit_z = if Int126.test_bit morton 2 then 1 lsl (depth - 1) else 0 in
      extract_bits
        (Int126.shift_right_logical morton 3)
        (depth - 1)
        (x_acc lor bit_x)
        (y_acc lor bit_y)
        (z_acc lor bit_z))
  in
  extract_bits morton precision 0 0 0
;;

(* Float coordinate conversion *)
let point_to_grid (p : Physics.point) (bounds : Bb.t) =
  let open Physics in
  let max_coord = (1 lsl precision) - 1 in
  let x_norm = (p.%{`x} -. bounds.x_min) /. (bounds.x_max -. bounds.x_min) in
  let y_norm = (p.%{`y} -. bounds.y_min) /. (bounds.y_max -. bounds.y_min) in
  let z_norm = (p.%{`z} -. bounds.z_min) /. (bounds.z_max -. bounds.z_min) in
  let x_grid = max 0 (min max_coord (int_of_float (x_norm *. float max_coord))) in
  let y_grid = max 0 (min max_coord (int_of_float (y_norm *. float max_coord))) in
  let z_grid = max 0 (min max_coord (int_of_float (z_norm *. float max_coord))) in
  x_grid, y_grid, z_grid
;;

let encode_point point bounds =
  let x_grid, y_grid, z_grid = point_to_grid point bounds in
  encode x_grid y_grid z_grid
;;
