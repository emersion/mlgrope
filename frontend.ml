open Graphics
open Unix

open Mlgrope

let ball_radius = 10.0

let draw_ball (b : ball) =
	Graphics.set_color Graphics.black;
	Graphics.fill_circle (int_of_float b.position.x) (int_of_float b.position.y) (int_of_float ball_radius)

let draw_bubble (b : bubble) =
	Graphics.set_color Graphics.red;
	Graphics.draw_circle (int_of_float b.position.x) (int_of_float b.position.y) (int_of_float b.radius)

let draw_rope (r : rope) =
	Graphics.set_color Graphics.green;
	Graphics.draw_circle (int_of_float r.position.x) (int_of_float r.position.y) (int_of_float r.radius)

let draw_entity e =
	match e with
	| Bubble(b) -> draw_bubble b
	| Rope(r) -> draw_rope r

let rec draw_entities l =
	match l with
	| e::l -> draw_entity e; draw_entities l
	| [] -> ()

let draw s =
	draw_ball s.ball;
	draw_entities s.entities

let rec loop g =
	let t = Unix.gettimeofday () in
	let dt = t -. g.time in
	let g = { g with time = t } in
	Graphics.clear_graph ();
	(* TODO: call backend *)
	draw g.state;
	Graphics.synchronize ();
	Unix.sleepf (1. /. 60.);
	loop g

let () =
	let g = {
		size = {x = 400.; y = 400.};
		time = Unix.gettimeofday ();
		state = {
			ball = {
				position = {x = 200.; y = 300.};
				speed = {x = 0.; y = 0.};
				accel = {x = 0.; y = 0.};
				links = [];
			};
			entities = [
				Bubble{position = {x = 100.; y = 200.}; radius = 50.};
				Rope{position = {x = 200.; y = 100.}; radius = 40.};
			];
		};
	} in
	Graphics.open_graph (" "^(string_of_int (int_of_float (g.size.x)))^"x"^(string_of_int (int_of_float (g.size.y))));
	Graphics.auto_synchronize false;
	loop g
