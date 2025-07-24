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
  let of_uint64 i = UInt64.{ high = zero; low = i }

  let add t1 t2 =
    let open UInt64.Infix in
    let low_result = t1.low + t2.low in
    (* Detect carry: if result < either operand, we had overflow *)
    let carry =
      if UInt64.compare low_result t1.low < 0 then UInt64.one else UInt64.zero
    in
    let high_result = t1.high + t2.high + carry in
    { high = high_result; low = low_result }
  ;;

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

  (* TODO: Fix *)
  let shift_right_logical t bits =
    match bits with
    | 0 -> t
    | bits when bits >= 64 -> UInt64.{ high = zero; low = shift_right t.high (bits - 64) }
    | bits ->
      let open UInt64.Infix in
      let low = (t.low lsr bits) lor (t.high lsl Stdlib.(64 - bits)) in
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

  let to_hex t =
    Printf.sprintf
      "0x%s"
      UInt64.(
        to_hexstring t.high ^ (to_hexstring t.low |> String.pad_left ~char:'0' ~len:16))
  ;;

  let of_hex s =
    (* read from the x add convert each char and then << 4 before next *)
    let value = ref zero in
    let s = String.split ~on:'x' s in
    let s = List.last_exn s in
    let rec loop = function
      | [] -> ()
      | c :: cs ->
        value := shift_left !value 4;
        let hex = String.of_char_list [ '0'; 'x'; c ] |> Int.Hex.of_string in
        value := add !value (of_int hex);
        loop cs
    in
    loop (String.to_list s);
    !value
  ;;

  let sexp_of_t t = Sexp.of_string (to_hex t)
  let t_of_sexp s = of_hex (Sexp.to_string s)

  let popcount t =
    let int64_popcount (i : uint64) =
      let open UInt64.Infix in
      let count = ref 0 in
      for k = 0 to 63 do
        (* add: ((1 << k) & i) >> k  *)
        count := (i land (UInt64.one lsl k)) lsr k |> UInt64.to_int |> Stdlib.( + ) !count
      done;
      !count
    in
    int64_popcount t.low + int64_popcount t.high
  ;;

  (* let t_of_sexp s = *)
end

type t = Int128.t

open Unsigned

let bits_per_dimension = 42 (* 128 / 3 *)

let encode (x : uint64) (y : uint64) (z : uint64) =
  let rec interleave acc (x : uint64) (y : uint64) (z : uint64) depth =
    match depth with
    | 0 -> acc
    | _ ->
      let open UInt64.Infix in
      let mask = UInt64.one lsl Int.(depth - 1) in
      let bit_x = x land mask in
      let bit_y = y land mask in
      let bit_z = z land mask in
      let acc_shifted = Int128.shift_left acc 3 in
      let bits_packed =
        UInt64.(
          (if not (equal bit_z zero) then one lsl 2 else zero)
          lor (if not (equal bit_y zero) then one lsl 1 else zero)
          lor if not (equal bit_x zero) then one else zero)
      in
      let combined = Int128.logor acc_shifted (Int128.of_uint64 bits_packed) in
      interleave combined x y z Int.(depth - 1)
  in
  interleave Int128.zero x y z bits_per_dimension
;;

let decode morton =
  let rec extract_bits morton depth x_acc y_acc z_acc =
    if depth = 0
    then x_acc, y_acc, z_acc
    else
      let open UInt64.Infix in
      let open UInt64 in
      let bit_x =
        if Int128.test_bit morton 0
        then one lsl Int.(bits_per_dimension - depth)
        else zero
      in
      let bit_y =
        if Int128.test_bit morton 1
        then one lsl Int.(bits_per_dimension - depth)
        else zero
      in
      let bit_z =
        if Int128.test_bit morton 2
        then one lsl Int.(bits_per_dimension - depth)
        else zero
      in
      (* print_endline (Int128.to_hex morton); *)
      (* printf "x: %d; y: %d; z: %d;\n" bit_x bit_y bit_z; *)
      (* printf "ACC: x: %d; y: %d; z: %d;\n" x_acc y_acc z_acc; *)
      extract_bits
        (Int128.shift_right_logical morton 3)
        Int.(depth - 1)
        (x_acc lor bit_x)
        (y_acc lor bit_y)
        (z_acc lor bit_z)
  in
  UInt64.(extract_bits morton bits_per_dimension zero zero zero)
;;

let uint64_of_float f =
  let open Float in
  if f < 0.0
  then UInt64.zero
  else if f < Float.int_pow 2.0 63
  then UInt64.of_int64 (Int64.of_float f)
  else (
    (* For floats >= 2^63, subtract 2^63, convert remainder, then add 2^63 back *)
    let f' = f -. Float.int_pow 2.0 63 in
    let remainder = UInt64.of_int64 (Int64.of_float f') in
    let two_to_63 = UInt64.shift_left UInt64.one 63 in
    UInt64.Infix.(two_to_63 + remainder))
;;

let float_of_uint64 u =
  let two_to_63 = UInt64.shift_left UInt64.one 63 in
  if UInt64.compare u two_to_63 < 0
  then
    (* Value < 2^63, can convert via Int64 *)
    Int64.to_float (UInt64.to_int64 u)
  else (
    (* Value >= 2^63, need to handle the high bit *)
    let remainder = UInt64.Infix.(u - two_to_63) in
    let remainder_float = Int64.to_float (UInt64.to_int64 remainder) in
    remainder_float +. Float.int_pow 2.0 63)
;;

let point_to_grid (p : Physics.point) (bounds : Bb.t) =
  let open Physics in
  let open UInt64 in
  let max_coord = Infix.((one lsl bits_per_dimension) - one) in
  let x_norm = (p.%{`x} -. bounds.x_min) /. (bounds.x_max -. bounds.x_min) in
  let y_norm = (p.%{`y} -. bounds.y_min) /. (bounds.y_max -. bounds.y_min) in
  let z_norm = (p.%{`z} -. bounds.z_min) /. (bounds.z_max -. bounds.z_min) in
  let x_grid =
    max zero (min max_coord (uint64_of_float (x_norm *. float_of_uint64 max_coord)))
  in
  let y_grid =
    max zero (min max_coord (uint64_of_float (y_norm *. float_of_uint64 max_coord)))
  in
  let z_grid =
    max zero (min max_coord (uint64_of_float (z_norm *. float_of_uint64 max_coord)))
  in
  x_grid, y_grid, z_grid
;;

let encode_point point bounds =
  let x_grid, y_grid, z_grid = point_to_grid point bounds in
  encode x_grid y_grid z_grid
;;

let parent_morton morton = Int128.shift_right_logical morton 3
let morton_at_level morton level = Int128.shift_right_logical morton (level * 3)
