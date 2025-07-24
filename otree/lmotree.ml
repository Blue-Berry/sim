open! Core
open Utils
module Physics = Body.Physics
module C = Centroid
module Int128 = Morton128.Int128

module MortonMap = Map.Make (struct
    type t = Int128.t

    let compare = Int128.compare
    let t_of_sexp = Int128.t_of_sexp
    let sexp_of_t = Int128.sexp_of_t
  end)

type region =
  { centroid : C.t
  ; bodies : int
  ; size : float
  }
[@@deriving sexp_of]

type t =
  { bodies : C.t list MortonMap.t
  ; regions : region MortonMap.t
  ; bounds : Bb.t
  ; theta : float (* Barnes-Hut approximation parameter *)
  }
[@@deriving sexp_of]

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
  let morton = Morton128.encode_point body.p tree.bounds in
  let existing_bodies = Map.find tree.bodies morton |> Option.value ~default:[] in
  let updated_bodies = Map.set tree.bodies ~key:morton ~data:(body :: existing_bodies) in
  { tree with bodies = updated_bodies }
;;

let insert_bodies bodies tree =
  List.fold bodies ~init:tree ~f:(fun acc particle -> insert_body particle acc)
;;

let build_aggregates tree =
  (* Use the same MortonMap that the tree uses *)
  let regions = ref MortonMap.empty in
  (* Calculate meaningful depth by finding the maximum Morton code *)
  let max_morton =
    Map.fold tree.bodies ~init:Int128.zero ~f:(fun ~key:morton ~data:_ acc ->
      if Int128.compare morton acc > 0 then morton else acc)
  in
  (* Calculate depth: how many 3-bit levels until we reach zero *)
  let meaningful_depth =
    let rec count_levels morton level =
      if Int128.equal morton Int128.zero
      then level - 1
      else count_levels (Int128.shift_right_logical morton 3) (level + 1)
    in
    max 1 (count_levels max_morton 1)
  in
  (* First pass: collect all bodies by their parent regions at each level *)
  Map.iteri tree.bodies ~f:(fun ~key:morton ~data:bodies ->
    List.iter bodies ~f:(fun body ->
      (* Track which regions we've already added this body to *)
      let seen_regions = ref Set.Poly.empty in
      for level = 1 to meaningful_depth do
        let region_morton = Morton128.morton_at_level morton level in
        (* Only add if we haven't seen this region for this body *)
        if not (Set.mem !seen_regions region_morton)
        then (
          seen_regions := Set.add !seen_regions region_morton;
          (* Get existing data for this region (bodies and level) *)
          let existing =
            Map.find !regions region_morton |> Option.value ~default:([], level)
          in
          let existing_bodies, stored_level = existing in
          (* Keep the minimum level (closest to root) *)
          let final_level = min level stored_level in
          regions
          := Map.set
               !regions
               ~key:region_morton
               ~data:(body :: existing_bodies, final_level))
      done));
  (* Second pass: calculate aggregate data for each region *)
  let final_regions =
    Map.mapi !regions ~f:(fun ~key:_ ~data:(bodies, level) ->
      let centroid = List.fold bodies ~init:C.empty ~f:(fun acc p -> C.add acc p) in
      { centroid
      ; bodies = List.length bodies
      ; size = region_size tree.bounds ~level ~bits_per_dimension:meaningful_depth
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
    (* Stop if we've gone too deep to prevent infinite loops *)
    if level < 0 then () else
    (* Try to find aggregate data at this level *)
    match Map.find tree.regions morton with
    | Some region_data ->
      let dist = distance target_body.pos region_data.centroid.p in
      (* Avoid division by zero *)
      if Float.(dist > 1e-10) then (
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
          (* Only traverse children that might have data *)
          for child_idx = 0 to 7 do
            let child_morton =
              let open Unsigned.UInt64.Infix in
              { Int128.high = (morton.high lsl 3) lor (morton.low lsr 60)
              ; low = (morton.low lsl 3) lor Unsigned.UInt64.of_int child_idx
              }
            in
            (* Only traverse if there's a chance of finding data *)
            if Map.mem tree.regions child_morton || Map.mem tree.bodies child_morton
            then traverse_tree child_morton (level - 1)
          done
      )
    | None ->
      (* No region data at this level - check if there are bodies directly *)
      (match Map.find tree.bodies morton with
      | Some bodies ->
        List.iter bodies ~f:(fun body ->
          if not (Physics.close_enough target_body.pos body.p)
          then (
            let force_vec = Physics.acc_on target_body.pos body.p body.m in
            total_force := Owl.Mat.(!total_force + force_vec)))
      | None -> 
        (* No data here at all - don't traverse children unnecessarily *)
        ())
  in
  (* Start traversal from root level with reasonable depth *)
  traverse_tree Int128.zero (min Morton128.bits_per_dimension 15);
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
