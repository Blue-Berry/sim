open! Core
module Physics = Body.Physics

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

  let int_of_octant (o : octant) : int =
    match o with
    | O1 -> 1
    | O2 -> 2
    | O3 -> 3
    | O4 -> 4
    | O5 -> 5
    | O6 -> 6
    | O7 -> 7
    | O8 -> 8
  ;;

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

(* Centroid *)
module C = struct
  type t =
    { p : Physics.point
    ; m : float
    }

  let empty = { p = Physics.zero; m = 0. }

  let add (c1 : t) (c2 : t) : t =
    let open Owl.Mat in
    let m : float = c1.m +. c2.m in
    let p : Physics.point = ((c1.m $* c1.p) + (c2.m $* c2.p)) /$ m in
    { p; m }
  ;;
end

type tree =
  { mutable size : int
  ; capacity : int
  ; mass_xyz : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t
  ; mass : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
  ; (* 8 x capacity *)
    children : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array2.t
  }

let create_tree ~capacity =
  { size = 0
  ; capacity
  ; mass_xyz = Bigarray.Array2.create Float32 C_layout 3 capacity
  ; mass = Bigarray.Array1.create Float32 C_layout capacity
  ; children = Bigarray.Array2.create Int32 C_layout 8 capacity
  }
;;

let has_children
      (arr : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array2.t)
      i
  =
  let open Int32 in
  List.init 8 ~f:(fun j -> arr.{j, i} > zero)
  |> List.fold ~init:false ~f:(fun acc x -> acc || x)
;;

let update_centroid (tree : tree) (node_index : int) (c : C.t) : unit =
  let open Physics in
  tree.mass_xyz.{0, node_index} <- c.p.%{`x};
  tree.mass_xyz.{1, node_index} <- c.p.%{`y};
  tree.mass_xyz.{2, node_index} <- c.p.%{`z};
  tree.mass.{node_index} <- c.m
;;

let get_centroid (tree : tree) (node_index : int) : C.t =
  let open Physics in
  let p =
    point
      tree.mass_xyz.{0, node_index}
      tree.mass_xyz.{1, node_index}
      tree.mass_xyz.{2, node_index}
  in
  C.{ p; m = tree.mass.{node_index} }
;;

let grow_tree (tree : tree) : tree =
  let new_capacity = tree.capacity * 2 in
  let new_tree = create_tree ~capacity:new_capacity in
  Bigarray.(
    Array2.blit tree.mass_xyz new_tree.mass_xyz;
    Array2.blit tree.children new_tree.children;
    Array1.blit tree.mass new_tree.mass);
  new_tree.size <- tree.size;
  new_tree
;;

let append_leaf (tree : tree) (c : C.t) : int =
  let tree = if tree.size = tree.capacity then grow_tree tree else tree in
  let node_index = tree.size in
  update_centroid tree node_index c;
  tree.size <- tree.size + 1;
  Int32.(
    for i = 0 to 7 do
      tree.children.{i, node_index} <- zero
    done);
  node_index
;;

let insert_body (tree : tree) (c : C.t) (bb : Bb.t) : unit =
  let rec insert node_index bb =
    assert (node_index < tree.capacity);
    match has_children tree.children node_index with
    | false (* leaf *) ->
      let c' = get_centroid tree node_index in
      update_centroid tree node_index (C.add c c');
      (* add previous leaf as a new child and add the new node to the list of children if they are in the same new child then recruse *)
      let child1_oct = Bb.octant_of_point c'.p bb in
      let child2_oct = Bb.octant_of_point c.p bb in
      if phys_equal child1_oct child2_oct
      then (
        let child1_index = append_leaf tree c' in
        tree.children.{Bb.int_of_octant child1_oct, node_index} <- Int32.of_int_exn child1_index;
        insert child1_index (Bb.octant_bb bb child1_oct))
      else
        tree.children.{Bb.int_of_octant child1_oct, node_index}
        <- append_leaf tree c' |> Int32.of_int_exn;
      tree.children.{Bb.int_of_octant child2_oct, node_index}
      <- append_leaf tree c |> Int32.of_int_exn
    | true (* internal *) -> failwith "TODO"
  in
  insert 0 bb
;;
