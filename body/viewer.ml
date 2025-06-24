open Graphics
open Body

let step_slow : float = 50.0
let dim = 1000
let dist = 4000000000.0

let float_to_screen (f : float) : int =
  int_of_float (f /. dist *. float_of_int (dim / 2)) + (dim / 2)
;;

let draw_centroid (m : float) (x : float) (y : float) (c : color) : unit =
  let px = float_to_screen x in
  let py = float_to_screen y in
  let r = if m > 2.0e28 then 10 else max (min (int_of_float (m /. 2.0e18)) 4) 2 in
  set_color c;
  fill_circle px py r
;;

let draw_body (b : Body.t) : unit =
  let open Body.Physics in
  draw_centroid b.mass b.pos.%{`x} b.pos.%{`y} white
;;

(* You can configure the program to run for a given number
   of frames before pausing. *)
let step_counter = { contents = None }
(*let step_counter = {contents=(Some 20)}*)

let should_pause () : bool =
  match step_counter.contents with
  | None -> false
  | Some n ->
    if n > 0 then step_counter.contents <- Some (n - 1);
    n = 0
;;

let run (bodies : Body.t list) (timestep : float) : unit =
  open_graph "";
  resize_window dim dim;
  auto_synchronize false;
  (* don't draw immediately to the screen *)
  while not (key_pressed ()) do
    clear_graph ();
    set_color black;
    fill_rect 0 0 dim dim;
    List.iter draw_body bodies;
    synchronize ();
    (* show the freshly painted window *)
    Body.step_bodies bodies timestep;
    (* When running continuously, a key press exits the program.
       * After exhuasting the frame budget, a quick press will single-step
       * and a held key will exit the program. *)
    if should_pause () then ignore (wait_next_event [ Key_pressed ]);
    ignore (wait_next_event [ Poll ]);
    (* Unix.sleepf 0.1; *)
    let sun = List.hd bodies in
    let open Body.Physics in
    let x = sun.pos.%{`x}
    and y = sun.pos.%{`y} in
    Printf.printf "sun: %f %f; mass: %f\n" x y sun.mass
  done
;;

run Body.Utils.planets step_slow
