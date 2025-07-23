open! Core
module Physics = Body.Physics

module Int128 = struct
  open Unsigned

  (* Use two native OCaml ints (63 bits each) for better performance *)
  type t =
    { high : UInt64.t
    ; low : UInt64.t
    }

  let zero = UInt64.{ high = zero; low = zero }
  let one = UInt64.{ high = zero; low = one }
  let of_int i = UInt64.{ high = zero; low = UInt64.of_int i }

  let shift_left t bits =
    match bits with
    | 0 -> t
    | bits when bits >= 64 ->
      UInt64.{ high = UInt64.shift_left t.low (bits - 64); low = zero }
    | bits ->
      let open UInt64.Infix in
      let high = (t.high lsl bits) lor (t.low lsr Stdlib.(64 - bits)) in
      let low = t.low lsl bits in
      { high; low }
  ;;

  let shift_right_logical t bits =
    match bits with
    | 0 -> t
    | bits when bits >= 64 -> UInt64.{ high = zero; low = shift_right t.high (bits - 64) }
    | bits ->
      let open UInt64.Infix in
      let low = (t.low lsr bits) lor (t.high lsr Stdlib.(64 - bits)) in
      let high = t.high lsr bits in
      { high; low }
  ;;

  let logand t1 t2 =
    UInt64.Infix.{ high = t1.high land t2.high; low = t1.low land t2.low }
  ;;

  let logor t1 t2 = UInt64.Infix.{ high = t1.high lor t2.high; low = t1.low lor t2.low }

  let logxor t1 t2 =
    UInt64.Infix.{ high = t1.high lxor t2.high; low = t1.low lxor t2.low }
  ;;

  let compare t1 t2 =
    let high_cmp = UInt64.compare t1.high t2.high in
    if high_cmp = 0 then UInt64.compare t1.low t2.low else high_cmp
  ;;

  let equal t1 t2 = UInt64.(equal t1.high t2.high && equal t1.low t2.low)

  let test_bit t bit_pos =
    let open UInt64.Infix in
    if bit_pos >= 64
    then (t.high lsr Stdlib.(bit_pos - 64)) land UInt64.one |> UInt64.equal UInt64.one
    else (t.low lsr bit_pos) land UInt64.one |> UInt64.equal UInt64.one
  ;;

  let to_hex t = Printf.sprintf "0x%s" UInt64.(to_hexstring t.high ^ to_hexstring t.low)
  let of_hex _s = failwith "TODO"
  let sexp_of_t t = Sexp.of_string (to_hex t)
  let t_of_sexp s = of_hex (Sexp.to_string s)

  (* let t_of_sexp s = *)
end

type t = Int128.t

let bits_per_dimension = 126 / 3

let encode x y z =
  let rec interleave acc x y z depth =
    match depth with
    | 0 -> acc
    | _ ->
      (* Extract bits using fast native operations *)
      let mask = 1 lsl (depth - 1) in
      let bit_x = x land mask in
      let bit_y = y land mask in
      let bit_z = z land mask in
      (* Pack bits efficiently *)
      let acc_shifted = Int128.shift_left acc 3 in
      let bits_packed =
        (if bit_z <> 0 then 4 else 0)
        lor (if bit_y <> 0 then 2 else 0)
        lor if bit_x <> 0 then 1 else 0
      in
      let combined = Int128.logor acc_shifted (Int128.of_int bits_packed) in
      interleave combined x y z (depth - 1)
  in
  interleave Int128.zero x y z bits_per_dimension
;;

let decode morton =
  let rec extract_bits morton depth x_acc y_acc z_acc =
    if depth = 0
    then x_acc, y_acc, z_acc
    else (
      let bit_x = if Int128.test_bit morton 0 then 1 lsl (depth - 1) else 0 in
      let bit_y = if Int128.test_bit morton 1 then 1 lsl (depth - 1) else 0 in
      let bit_z = if Int128.test_bit morton 2 then 1 lsl (depth - 1) else 0 in
      extract_bits
        (Int128.shift_right_logical morton 3)
        (depth - 1)
        (x_acc lor bit_x)
        (y_acc lor bit_y)
        (z_acc lor bit_z))
  in
  extract_bits morton bits_per_dimension 0 0 0
;;

let point_to_grid (p : Physics.point) (bounds : Bb.t) =
  let open Physics in
  let max_coord = (1 lsl bits_per_dimension) - 1 in
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

let parent_morton morton = Int128.shift_right_logical morton 3
let morton_at_level morton level = Int128.shift_right_logical morton (level * 3)
