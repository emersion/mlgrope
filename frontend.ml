open Graphics
open Sys
open Unix

open Mlgrope
open Backend

let tick_rate = 1. /. 60.

let ball_color = Graphics.black
let bubble_color = Graphics.red
let rope_color = Graphics.green
let goal_color = Graphics.blue

let mix v1 v2 t =
	{ x = t *. v1.x +. (1. -. t) *. v2.x; y = t *. v1.y +. (1. -. t) *. v2.y }

let int_of_position p =
	(int_of_float p.x, int_of_float p.y)

let draw_ball (b : ball) =
	let (x, y) = int_of_position b.position in
	Graphics.set_color ball_color;
	Graphics.fill_circle x y (int_of_float Mlgrope.ball_radius)

let draw_bubble (b : bubble) =
	let (x, y) = int_of_position b.position in
	Graphics.set_color bubble_color;
	Graphics.draw_circle x y (int_of_float b.radius)

let draw_rope (r : rope) =
	let (x, y) = int_of_position r.position in
	Graphics.set_color rope_color;
	Graphics.draw_circle x y (int_of_float r.radius)

let draw_goal (g : goal) =
	let (x, y) = int_of_position g.position in
	Graphics.set_color goal_color;
	Graphics.fill_rect (x - 1) (y - 1) 2 2

let draw_entity e =
	match e with
	| Bubble(b) -> draw_bubble b
	| Rope(r) -> draw_rope r
	| Goal(g) -> draw_goal g

let rec draw_link b l =
	match l with
	| Rope({position}) -> let a = 0.1 in
		let n = 10 in
		let line = Array.init (n+1) (fun i ->
			let t = (float_of_int i) /. (float_of_int n) in
			int_of_position (mix position b.position t)
			(* TODO *)
		) in
		Graphics.set_color Graphics.black;
		Graphics.draw_poly_line line
	| _ -> ()

let draw s =
	draw_ball s.ball;
	List.iter draw_entity s.entities;
	List.iter (draw_link s.ball) s.ball.links

let step g =
	let t = Unix.gettimeofday () in
	let dt = t -. g.time in
	let g = { g with time = t } in
	Graphics.clear_graph ();
	let g = { g with state = Backend.move g.state dt } in
	draw g.state;
	Graphics.synchronize ();
	g

let () =
	let rope = {position = {x = 300.; y = 100.}; radius = 40.} in
	let g = {
		size = {x = 400.; y = 400.};
		time = Unix.gettimeofday ();
		state = {
			ball = {
				position = {x = 200.; y = 300.};
				speed = {x = 0.; y = 0.};
				accel = {x = 0.; y = -150.};
				links = [Rope(rope)];
			};
			entities = [
				Bubble{position = {x = 100.; y = 200.}; radius = 50.};
				Rope(rope);
				Goal{position = {x = 200.; y = 20.}};
			];
		};
	} in
	Graphics.open_graph (" "^(string_of_int (int_of_float (g.size.x)))^"x"^(string_of_int (int_of_float (g.size.y))));
	Graphics.auto_synchronize false;

	let g = ref (step g) in
	Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> g := step !g));
	let _ = Unix.setitimer Unix.ITIMER_REAL {it_interval = tick_rate; it_value = tick_rate} in
	Graphics.loop_at_exit [] (fun _ -> ())
