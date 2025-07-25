open! Core
module Physics = Body.Physics
open Utils
module C = Centroid
module Bb = Bb

type tree =
  { mutable size : int
  ; mutable capacity : int
  ; mutable mass_xyz : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t
  ; mutable mass : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
  ; (* 8 x capacity *)
    mutable children : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array2.t
  ; theta : float (* Barnes-Hut approximation parameter *)
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

let create_tree ~capacity ~theta =
  { size = 0
  ; capacity
  ; mass_xyz = Bigarray.Array2.init Float32 C_layout capacity 3 (fun _ _ -> 0.)
  ; mass = Bigarray.Array1.init Float32 C_layout capacity (fun _ -> 0.)
  ; children = Bigarray.Array2.init Int32 C_layout capacity 8 (fun _ _ -> Int32.zero)
  ; theta
  }
;;

let has_children
      (arr : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array2.t)
      i
  =
  let open Int32 in
  List.init 8 ~f:(fun j -> arr.{i, j} > zero)
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

let grow_tree (tree : tree) : unit =
  let old_capacity = tree.capacity in
  let new_capacity = old_capacity * 2 in
  let new_mass_xyz = Bigarray.Array2.init Float32 C_layout new_capacity 3 (fun _ _ -> 0.) in
  let new_mass = Bigarray.Array1.init Float32 C_layout new_capacity (fun _ -> 0.) in
  let new_children = Bigarray.Array2.init Int32 C_layout new_capacity 8 (fun _ _ -> Int32.zero) in
  
  Bigarray.(
    Array2.blit tree.mass_xyz (Array2.sub_left new_mass_xyz 0 old_capacity);
    Array2.blit tree.children (Array2.sub_left new_children 0 old_capacity);
    Array1.blit tree.mass (Array1.sub new_mass 0 old_capacity));
  
  tree.capacity <- new_capacity;
  tree.mass_xyz <- new_mass_xyz;
  tree.mass <- new_mass;
  tree.children <- new_children
;;

let append_leaf (tree : tree) (c : C.t) : int =
  if tree.size = tree.capacity then grow_tree tree;
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

(* Distance between two points *)
let[@inline] distance (p1 : Physics.vec) (p2 : Physics.vec) =
  let open Physics in
  p1 --> p2 |> mag
;;

(* Calculate size of bounding box *)
let[@inline] bb_size (bb : Bb.t) =
  Float.max
    Bb.(bb.x_max -. bb.x_min)
    (Float.max Bb.(bb.y_max -. bb.y_min) Bb.(bb.z_max -. bb.z_min))
;;

(* Barnes-Hut force calculation using iterative traversal to avoid stack overflow *)
let calculate_force_on_body (target_body : Body.t) (tree : tree) (bb : Bb.t) =
  let total_force = ref (Physics.vec 0. 0. 0.) in
  let work_queue = Queue.create () in
  Queue.enqueue work_queue (0, bb);
  
  let rec process_queue () =
    match Queue.dequeue work_queue with
    | None -> ()
    | Some (node_index, node_bb) ->
        if node_index >= tree.size then
          process_queue ()
        else (
          let centroid = get_centroid tree node_index in
          (* Skip if centroid has no mass *)
          if Float.(centroid.m > 0.) then (
            let dist = distance target_body.pos centroid.p in
            (* Avoid division by zero and self-interaction *)
            if Float.(dist > 1e-10) && not (Physics.close_enough target_body.pos centroid.p) then (
              let node_size = bb_size node_bb in
              let ratio = node_size /. dist in
              
              match has_children tree.children node_index with
              | false (* leaf *) ->
                  (* For leaf nodes, always calculate direct force *)
                  let force_vec = Physics.acc_on target_body.pos centroid.p centroid.m in
                  total_force := Owl.Mat.(!total_force + force_vec);
                  process_queue ()
              | true (* internal *) ->
                  (* Barnes-Hut criterion: if s/d < θ, use approximation *)
                  if Float.(ratio < tree.theta) then (
                    (* Use aggregate mass - treat whole region as single body *)
                    let force_vec = Physics.acc_on target_body.pos centroid.p centroid.m in
                    total_force := Owl.Mat.(!total_force + force_vec);
                    process_queue ()
                  ) else (
                    (* Too close - need to traverse children *)
                    for child_idx = 0 to 7 do
                      let child_node_index = tree.children.{node_index, child_idx} in
                      if Int32.(child_node_index > zero) then
                        let child_octant = Bb.octant_of_int child_idx in
                        let child_bb = Bb.octant_bb node_bb child_octant in
                        Queue.enqueue work_queue (Int32.to_int_exn child_node_index, child_bb)
                    done;
                    process_queue ()
                  )
            ) else 
              process_queue ()
          ) else
            process_queue ()
        )
  in
  process_queue ();
  !total_force
;;

(* Calculate forces on all bodies *)
let calculate_all_forces (tree : tree) (bodies : Body.t list) (bb : Bb.t) =
  List.map bodies ~f:(fun body ->
    let force = calculate_force_on_body body tree bb in
    body, force)
;;

(* Update particle positions based on forces (simple Euler integration) *)
let update_simulation bodies_forces dt =
  List.iter bodies_forces ~f:(fun ((body : Body.t), (force : Physics.vec)) ->
    let acceleration = Owl.Mat.(force /$ body.mass) in
    body.vel <- Owl.Mat.(body.vel + (acceleration *$ dt));
    body.pos <- Owl.Mat.(body.pos + (body.vel *$ dt)))
;;

(* For dynamic simulations where bodies move each timestep *)
let simulation_timestep bodies bounds ~theta ~dt =
  (* Rebuild tree from scratch each timestep - this is standard practice *)
  let empty_tree = create_tree ~capacity:1000 ~theta in
  let centroids =
    List.map bodies ~f:(fun (body : Body.t) -> C.{ m = body.mass; p = body.pos })
  in
  List.iter centroids ~f:(fun c -> insert_body empty_tree c bounds);
  let forces = calculate_all_forces empty_tree bodies bounds in
  update_simulation forces dt
;;
