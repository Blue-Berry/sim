open! Core
module Physics = Body.Physics

(* Centroid *)
module C = struct
  type t =
    { p : Physics.point
    ; m : float
    }
end

(* Bounding box *)
module Bb = struct
  type t =
    { x_min : float
    ; x_max : float
    ; y_min : float
    ; y_max : float
    ; z_min : float
    ; z_max : float
    }
  [@@deriving sexp_of]

  type octant =
    (* xyz *)
    | O1 (* +++ *)
    | O2 (* -++ *)
    | O3 (* +-+ *)
    | O4 (* --+ *)
    | O5 (* ++- *)
    | O6 (* -+- *)
    | O7 (* +-- *)
    | O8 (* --- *)
  [@@deriving sexp_of]

  let octant_of_point (p : Body.Physics.point) (bb : t) : octant =
    let open Body.Physics in
    let open Float in
    let x = p.%{`x} in
    let y = p.%{`y} in
    let z = p.%{`z} in
    assert (x <= bb.x_max && x >= bb.x_min);
    assert (y <= bb.y_max && y >= bb.y_min);
    assert (z <= bb.z_max && z >= bb.z_min);
    let x_mid = (bb.x_max +. bb.x_min) /. 2. in
    let y_mid = (bb.y_max +. bb.y_min) /. 2. in
    let z_mid = (bb.z_max +. bb.z_min) /. 2. in
    let sign x = if x < 0. then -1 else 1 in
    match sign (x -. x_mid), sign (y -. y_mid), sign (z -. z_mid) with
    | 1, 1, 1 -> O1
    | -1, 1, 1 -> O2
    | 1, -1, 1 -> O3
    | -1, -1, 1 -> O4
    | 1, 1, -1 -> O5
    | -1, 1, -1 -> O6
    | 1, -1, -1 -> O7
    | -1, -1, -1 -> O8
    | _ -> failwith "Invalid octant"
  ;;

  let octant_bb (bb : t) (o : octant) : t =
    let[@inline] x_mid () = (bb.x_max +. bb.x_min) /. 2. in
    let[@inline] y_mid () = (bb.y_max +. bb.y_min) /. 2. in
    let[@inline] z_mid () = (bb.z_max +. bb.z_min) /. 2. in
    let x_min, x_max, y_min, y_max, z_min, z_max =
      match o with
      | O1 -> x_mid (), bb.x_max, y_mid (), bb.y_max, z_mid (), bb.z_max
      | O2 -> bb.x_min, x_mid (), y_mid (), bb.y_max, z_mid (), bb.z_max
      | O3 -> x_mid (), bb.x_max, bb.y_min, y_mid (), z_mid (), bb.z_max
      | O4 -> bb.x_min, x_mid (), bb.y_min, y_mid (), z_mid (), bb.z_max
      | O5 -> x_mid (), bb.x_max, y_mid (), bb.y_max, bb.z_min, z_mid ()
      | O6 -> bb.x_min, x_mid (), y_mid (), bb.y_max, bb.z_min, z_mid ()
      | O7 -> x_mid (), bb.x_max, bb.y_min, y_mid (), bb.z_min, z_mid ()
      | O8 -> bb.x_min, x_mid (), bb.y_min, y_mid (), bb.z_min, z_mid ()
    in
    { x_min; x_max; y_min; y_max; z_min; z_max }
  ;;

  let parent_bb (bb : t) (o : octant) : t =
    let[@inline] dx () = bb.x_max -. bb.x_min in
    let[@inline] dy () = bb.y_max -. bb.y_min in
    let[@inline] dz () = bb.z_max -. bb.z_min in
    match o with
    | O1 ->
      { bb with
        x_min = bb.x_min -. dx ()
      ; y_min = bb.y_min -. dy ()
      ; z_min = bb.z_min -. dz ()
      }
    | O2 ->
      { bb with
        x_max = bb.x_max +. dx ()
      ; y_min = bb.y_min -. dy ()
      ; z_min = bb.z_min -. dz ()
      }
    | O3 ->
      { bb with
        x_min = bb.x_min -. dx ()
      ; y_max = bb.y_max +. dy ()
      ; z_min = bb.z_min -. dz ()
      }
    | O4 ->
      { bb with
        x_max = bb.x_max +. dx ()
      ; y_max = bb.y_max +. dy ()
      ; z_min = bb.z_min -. dz ()
      }
    | O5 ->
      { bb with
        x_min = bb.x_min -. dx ()
      ; y_min = bb.y_min -. dy ()
      ; z_max = bb.z_max +. dz ()
      }
    | O6 ->
      { bb with
        x_max = bb.x_max +. dx ()
      ; y_min = bb.y_min -. dy ()
      ; z_max = bb.z_max +. dz ()
      }
    | O7 ->
      { bb with
        x_min = bb.x_min -. dx ()
      ; y_max = bb.y_max +. dy ()
      ; z_max = bb.z_max +. dz ()
      }
    | O8 ->
      { bb with
        x_max = bb.x_max +. dx ()
      ; y_max = bb.y_max +. dy ()
      ; z_max = bb.z_max +. dz ()
      }
  ;;
end

type t =
  | Node of (C.t * t)
  | Leaf of (C.t * t)
  | Empty of t
  | Root of (Bb.t * t)
  | End

let empty bb = Root (bb, End)
