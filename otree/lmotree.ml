open! Core
module Physics = Body.Physics
module C = Centroid
module Int126 = Morton126.Int126

module MortonMap = Map.Make (struct
    type t = Int126.t

    let compare = Int126.compare
    let t_of_sexp = Int126.t_of_sexp
    let sexp_of_t = Int126.sexp_of_t
  end)

type region =
  { centroid : C.t
  ; bodies : int
  ; size : float
  }

type t =
  { bodies : C.t list MortonMap.t
  ; regions : region MortonMap.t
  ; bounds : Bb.t
  ; theta : float (* Barnes-Hut approximation parameter *)
  }

(* Calculate physical size of a region at given level *)
let region_size (bounds : Bb.t) ~level ~bits_per_dimension =
  let total_size =
    Float.max
      Bb.(bounds.x_max -. bounds.x_min)
      Bb.(Float.max (bounds.y_max -. bounds.y_min) (bounds.z_max -. bounds.z_min))
  in
  total_size /. Owl.Maths.pow 2.0 (Float.of_int (bits_per_dimension - level))
;;

(* Create empty tree *)
let create bounds theta =
  { bodies = MortonMap.empty; regions = MortonMap.empty; bounds; theta }
;;

(* Insert particle (just add to leaf - don't calculate aggregates yet) *)
let insert_body (body : C.t) tree =
  let morton = Morton126.encode_point body.p tree.bounds in
  let existing_bodies = Map.find tree.bodies morton |> Option.value ~default:[] in
  let updated_bodies = Map.set tree.bodies ~key:morton ~data:(body :: existing_bodies) in
  { tree with bodies = updated_bodies }
;;

let insert_bodies bodies tree =
  List.fold bodies ~init:tree ~f:(fun acc particle -> insert_body particle acc)
;;

(* Build aggregates ONCE after all bodies are inserted *)
let build_aggregates tree =
  let regions = ref MortonMap.empty in
  (* First pass: collect all bodies by their parent regions at each level *)
  Map.iteri tree.bodies ~f:(fun ~key:morton ~data:bodies ->
    List.iter bodies ~f:(fun body ->
      (* Update all levels from leaf to root *)
      for level = 1 to Morton126.bits_per_dimension do
        let region_morton = Morton126.morton_at_level morton level in
        let existing = Map.find !regions region_morton |> Option.value ~default:[] in
        regions := Map.set !regions ~key:region_morton ~data:(body :: existing)
      done));
  (* Second pass: calculate aggregate data for each region *)
  let final_regions =
    Map.mapi !regions ~f:(fun ~key:region_morton ~data:bodies ->
      let centroid = List.fold bodies ~init:C.empty ~f:(fun acc p -> C.add acc p) in
      let level =
        Morton126.bits_per_dimension
        - ((Int.popcount region_morton.high + Int.popcount region_morton.low) / 3)
      in
      { centroid
      ; bodies = List.length bodies
      ; size =
          region_size tree.bounds ~level ~bits_per_dimension:Morton126.bits_per_dimension
      })
  in
  { tree with regions = final_regions }
;;

(* Distance between two points *)
let[@inline] distance (p1 : Physics.vec) (p2 : Physics.vec) =
  let open Physics in
  p1 --> p2 |> mag
;;

(* Barnes-Hut force calculation *)
let calculate_force_on_body (target_body : Body.t) tree =
  let total_force = ref (Physics.vec 0. 0. 0.) in
  let rec traverse_tree morton level =
    (* Try to find aggregate data at this level *)
    match Map.find tree.regions morton with
    | Some region_data ->
      let dist = distance target_body.pos region_data.centroid.p in
      let ratio = region_data.size /. dist in
      (* Barnes-Hut criterion: if s/d < θ, use approximation *)
      if Float.(ratio < tree.theta)
      then (
        (* Use aggregate mass - treat whole region as single body *)
        let force_vec =
          Physics.acc_on target_body.pos region_data.centroid.p region_data.centroid.m
        in
        total_force := Owl.Mat.(!total_force + force_vec))
      else if level = 0 (* Region too close - need to go deeper *)
      then (
        (* At leaf level - calculate individual body forces *)
        match Map.find tree.bodies morton with
        | Some bodies ->
          List.iter bodies ~f:(fun body ->
            if not (Physics.close_enough target_body.pos body.p)
            then (
              let force_vec = Physics.acc_on target_body.pos body.p body.m in
              total_force := Owl.Mat.(!total_force + force_vec)))
        | None -> ())
      else
        for
          (* Go to child level - examine 8 children *)
          child_idx = 0 to 7
        do
          let child_morton =
            { Int126.high = (morton.high lsl 3) lor (morton.low lsr 60)
            ; low = (morton.low lsl 3) lor child_idx land Int126.mask_63
            }
          in
          traverse_tree child_morton (level - 1)
        done
    | None ->
      (* No data at this level, try going to leaf level *)
      if level > 0
      then
        for child_idx = 0 to 7 do
          let child_morton =
            { Int126.high = (morton.high lsl 3) lor (morton.low lsr 60)
            ; low = (morton.low lsl 3) lor child_idx land Int126.mask_63
            }
          in
          traverse_tree child_morton (level - 1)
        done
  in
  (* Start traversal from root level *)
  traverse_tree Int126.zero Morton126.bits_per_dimension;
  !total_force
;;

(* Calculate forces on all bodies *)
let calculate_all_forces tree bodies =
  List.map bodies ~f:(fun body ->
    let force = calculate_force_on_body body tree in
    body, force)
;;

(* Update particle positions based on forces (simple Euler integration) *)
let update_simulation bodies_forces dt =
  List.iter bodies_forces ~f:(fun ((body : Body.t), (force : Physics.vec)) ->
    let acceleration = Owl.Mat.(force /$ body.mass) in
    body.vel <- Owl.Mat.(body.vel + (acceleration *$ dt));
    body.pos <- Owl.Mat.(body.pos + (body.vel *$ dt)))
;;

(* Usage example with efficient batch processing *)
let gravity_simulation_example () =
  let bounds =
    Bb.
      { x_min = -1000.0
      ; x_max = 1000.0
      ; y_min = -1000.0
      ; y_max = 1000.0
      ; z_min = -1000.0
      ; z_max = 1000.0
      }
  in
  (* Create empty tree *)
  let tree = create bounds 0.5 in
  (* Add many bodies efficiently *)
  let bodies =
    let open Physics in
    Body.
      [ { pos = point 0. 0. 0.; mass = 1000.0; vel = vec 0. 0. 0. }
      ; { pos = point 100. 0. 0.; mass = 1000.0; vel = vec 0. 10. 0. }
      ; { pos = point (-100.) 0. 0.; mass = 1000.0; vel = vec 0. (-10.) 0. }
      ]
  in
  let centroids =
    List.map bodies ~f:(fun (body : Body.t) -> C.{ m = body.mass; p = body.pos })
  in
  (* EFFICIENT: Batch insert all bodies first (fast) *)
  let tree_with_bodies = insert_bodies centroids tree in
  (* THEN: Build all aggregates once (single pass) *)
  let tree_with_aggregates = build_aggregates tree_with_bodies in
  (* Now ready for force calculations *)
  let forces = calculate_all_forces tree_with_aggregates bodies in
  update_simulation forces 0.01;
  Printf.printf "Efficient gravity simulation:\n";
  Printf.printf "1. Batch inserted %d bodies (O(N))\n" (List.length bodies);
  Printf.printf "2. Built aggregates once (O(N))\n";
  Printf.printf "3. Calculated forces using Barnes-Hut (O(N log N))\n";
  tree_with_aggregates
;;

(* For dynamic simulations where bodies move each timestep *)
let simulation_timestep bodies bounds ~theta ~dt =
  (* Rebuild tree from scratch each timestep - this is standard practice *)
  let empty_tree = create bounds theta in
  let centroids =
    List.map bodies ~f:(fun (body : Body.t) -> C.{ m = body.mass; p = body.pos })
  in
  let tree_with_bodies = insert_bodies centroids empty_tree in
  let tree_with_aggregates = build_aggregates tree_with_bodies in
  let forces = calculate_all_forces tree_with_aggregates bodies in
  update_simulation forces dt
;;
