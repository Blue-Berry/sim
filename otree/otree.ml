open! Core
module Physics = Body.Physics
module Bb = Bb

(* Centroid *)
module C = struct
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
end

type tree =
  { mutable size : int
  ; capacity : int
  ; mass_xyz : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t
  ; mass : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
  ; (* 8 x capacity *)
    children : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array2.t
  }

let print_tree (tree : tree) =
  let open Format in
  let centroids =
    List.init tree.size ~f:(fun i ->
      let p =
        Physics.point tree.mass_xyz.{i, 0} tree.mass_xyz.{i, 1} tree.mass_xyz.{i, 2}
      in
      let m = tree.mass.{i} in
      C.{ p; m })
    |> List.foldi ~init:"" ~f:(fun i acc c ->
      acc ^ "\n" ^ Format.asprintf "%d)\t%a" i C.pp c)
  in
  fprintf
    std_formatter
    "Tree:\nsize: %d\ncapacity: %d\ncentroids: %s\nchildren: %s\n"
    tree.size
    tree.capacity
    centroids
    (List.init tree.size ~f:(fun i ->
       List.init 8 ~f:(fun j -> tree.children.{i, j} |> Int32.to_string)
       |> List.rev
       |> List.fold ~init:" " ~f:(fun c acc -> acc ^ "-" ^ c))
     |> List.foldi ~init:"" ~f:(fun i s acc -> s ^ "\n" ^ string_of_int i ^ ")\t" ^ acc))
;;

let create_tree ~capacity =
  { size = 0
  ; capacity
  ; mass_xyz = Bigarray.Array2.init Float32 C_layout capacity 3 (fun _ _ -> 0.)
  ; mass = Bigarray.Array1.init Float32 C_layout capacity (fun _ -> 0.)
  ; children = Bigarray.Array2.init Int32 C_layout capacity 8 (fun _ _ -> Int32.zero)
  }
;;

let has_children
      (arr : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array2.t)
      i
  =
  let open Int32 in
  List.init 7 ~f:(fun j -> arr.{i, j} > zero)
  |> List.fold ~init:false ~f:(fun acc x -> acc || x)
;;

let update_centroid (tree : tree) (node_index : int) (c : C.t) : unit =
  let open Physics in
  tree.mass_xyz.{node_index, 0} <- c.p.%{`x};
  tree.mass_xyz.{node_index, 1} <- c.p.%{`y};
  tree.mass_xyz.{node_index, 2} <- c.p.%{`z};
  tree.mass.{node_index} <- c.m
;;

let get_centroid (tree : tree) (node_index : int) : C.t =
  let open Physics in
  let p =
    point
      tree.mass_xyz.{node_index, 0}
      tree.mass_xyz.{node_index, 1}
      tree.mass_xyz.{node_index, 2}
  in
  C.{ p; m = tree.mass.{node_index} }
;;

let grow_tree (tree : tree) : tree =
  let new_capacity = tree.capacity * 2 in
  let new_tree = create_tree ~capacity:new_capacity in
  Bigarray.(
    assert (phys_equal (Array2.dim2 tree.mass_xyz) (Array2.dim2 new_tree.mass_xyz));
    printf
      "mass_xyz size: %d %d; Capacity: %d\n"
      (Array2.dim1 tree.mass_xyz)
      (Array2.dim2 tree.mass_xyz)
      new_tree.capacity;
    assert (phys_equal (Array2.dim1 new_tree.mass_xyz) new_tree.capacity);
    assert (phys_equal (Array2.dim2 new_tree.children) (Array2.dim2 new_tree.children));
    assert (phys_equal (Array2.dim1 new_tree.children) new_tree.capacity);
    assert (phys_equal (Array1.dim new_tree.mass) new_tree.capacity);
    Array2.blit tree.mass_xyz (Array2.sub_left new_tree.mass_xyz 0 tree.capacity);
    Array2.blit tree.children (Array2.sub_left new_tree.children 0 tree.capacity);
    Array1.blit tree.mass (Array1.sub new_tree.mass 0 tree.capacity));
  new_tree.size <- tree.size;
  new_tree
;;

let append_leaf (tree : tree) (c : C.t) : int =
  let tree = if tree.size = tree.capacity then grow_tree tree else tree in
  let node_index = tree.size in
  update_centroid tree node_index c;
  tree.size <- tree.size + 1;
  node_index
;;

let insert_body (tree : tree) (c : C.t) (bb : Bb.t) : unit =
  let rec insert node_index bb =
    assert (node_index < tree.capacity);
    match has_children tree.children node_index with
    | false (* leaf *) ->
      let c' = get_centroid tree node_index in
      if Float.(c'.m = 0.) (*check if leaf is empty*)
      then (
        tree.size <- tree.size + 1;
        update_centroid tree node_index c)
      else (
        (* Turns leaf into internal node *)
        let leaf_index = append_leaf tree c' in
        tree.children.{node_index, Bb.(octant_of_point c'.p bb |> int_of_octant)}
        <- Int32.of_int_exn leaf_index;
        (* if node_index > 1 then failwithf "node_index == %d; Has_children: %b" node_index (has_children tree.children node_index) (); *)
        insert node_index bb)
    | true (* internal *) ->
      update_centroid tree node_index (C.add c (get_centroid tree node_index));
      let octant = Bb.octant_of_point c.p bb in
      let octant_idx = Bb.int_of_octant octant in
      if Int32.equal tree.children.{node_index, octant_idx} Int32.zero
      then (
        let leaf_index = append_leaf tree c in
        tree.children.{node_index, octant_idx} <- Int32.of_int_exn leaf_index)
      else
        insert
          (Int32.to_int_exn tree.children.{node_index, octant_idx})
          (Bb.octant_bb bb octant)
  in
  insert 0 bb
;;

module LinearOctree = struct
  module Int126 = Morton126.Int126

  module MortonMap = Stdlib.Map.Make (struct
      type t = Int126.t

      let compare = Int126.compare
    end)

  type 'a entry =
    { point : Physics.point
    ; value : 'a
    }

  type 'a t =
    { data : 'a entry list MortonMap.t
    ; bounds : Bb.t
    }

  (* Create empty octree *)
  let create bounds = { data = MortonMap.empty; bounds }

  (* Insert a point *)
  let insert point value octree =
    let morton = Morton126.encode_point point octree.bounds in
    let entry = { point; value } in
    let existing =
      match MortonMap.find_opt morton octree.data with
      | None -> []
      | Some entries -> entries
    in
    let updated_data = MortonMap.add morton (entry :: existing) octree.data in
    { octree with data = updated_data }
  ;;

  (* Range query *)
  let range_query (query_bounds : Bb.t) octree =
    let min_point =
      Physics.point query_bounds.x_min query_bounds.y_min query_bounds.z_min
    in
    let max_point =
      Physics.point query_bounds.x_min query_bounds.y_min query_bounds.z_min
    in
    let min_morton = Morton126.encode_point min_point octree.bounds in
    let max_morton = Morton126.encode_point max_point octree.bounds in
    MortonMap.fold
      (fun morton entries acc ->
         if Int126.compare morton min_morton >= 0 && Int126.compare morton max_morton <= 0
         then (
           let filtered =
             List.filter
               ~f:(fun entry ->
                 let open Physics in
                 let open Float in
                 entry.point.%{`x} >= query_bounds.x_min
                 && entry.point.%{`x} <= query_bounds.x_max
                 && entry.point.%{`y} >= query_bounds.y_min
                 && entry.point.%{`y} <= query_bounds.y_max
                 && entry.point.%{`z} >= query_bounds.z_min
                 && entry.point.%{`z} <= query_bounds.z_max)
               entries
           in
           List.rev_append filtered acc)
         else acc)
      octree.data
      []
  ;;

  (* Nearest neighbor search *)
  let nearest_neighbor query_point k octree =
    let distance_squared p1 p2 =
      let open Physics in
      let dx = p1.%{`x} -. p2.%{`x} in
      let dy = p1.%{`y} -. p2.%{`y} in
      let dz = p1.%{`z} -. p2.%{`z} in
      (dx *. dx) +. (dy *. dy) +. (dz *. dz)
    in
    let all_entries =
      MortonMap.fold (fun _ entries acc -> List.rev_append entries acc) octree.data []
    in
    let sorted =
      List.sort
        ~compare:(fun a b ->
          let dist_a = distance_squared query_point a.point in
          let dist_b = distance_squared query_point b.point in
          Float.compare dist_a dist_b)
        all_entries
    in
    let rec take n lst acc =
      match lst, n with
      | _, 0 -> List.rev acc
      | [], _ -> List.rev acc
      | h :: t, n -> take (n - 1) t (h :: acc)
    in
    take k sorted []
  ;;

  (* Bulk insert *)
  let bulk_insert points_values octree =
    List.fold_left
      ~f:(fun acc (point, value) -> insert point value acc)
      ~init:octree
      points_values
  ;;

  (* Get all entries *)
  let to_list octree =
    MortonMap.fold (fun _ entries acc -> List.rev_append entries acc) octree.data []
  ;;

  (* Size *)
  let size octree =
    MortonMap.fold (fun _ entries acc -> acc + List.length entries) octree.data 0
  ;;
end

(* Usage example *)
let example_usage () =
  (* Create 3D octree with bounds *)
  let bounds =
    { Bb.x_min = 0.0
    ; x_max = 1000.0
    ; y_min = 0.0
    ; y_max = 1000.0
    ; z_min = 0.0
    ; z_max = 1000.0
    }
  in
  let octree = LinearOctree.create bounds in
  (* 20 bits per dimension *)
  (* Insert some points *)
  let points =
    let open Physics in
    [ point 100.5 200.3 300.7, "point1"
    ; point 150.2 250.8 350.1, "point2"
    ; point 500.0 500.0 500.0, "center"
    ]
  in
  let octree_with_data = LinearOctree.bulk_insert points octree in
  (* Range query *)
  let query_bounds =
    { Bb.x_min = 90.0
    ; x_max = 200.0
    ; y_min = 190.0
    ; y_max = 300.0
    ; z_min = 290.0
    ; z_max = 400.0
    }
  in
  let results = LinearOctree.range_query query_bounds octree_with_data in
  (* Nearest neighbor *)
  let query_point = Physics.point 100.0 200.0 300.0 in
  let nearest = LinearOctree.nearest_neighbor query_point 2 octree_with_data in
  octree_with_data, results, nearest
;;
