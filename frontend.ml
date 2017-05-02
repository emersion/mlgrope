open Graphics
open Unix

open Mlgrope

let rec loop g =
	let t = Unix.gettimeofday () in
	let dt = t -. g.time in
	let g = { g with time = t } in
	Graphics.clear_graph ();
	(* TODO: call backend *)
	(* TODO: draw *)
	Graphics.synchronize ();
	Unix.sleepf (1. /. 60.);
	loop g

let () =
	let g = {
		size = {x = 200.; y = 200.};
		time = Unix.gettimeofday ();
		state = {
			ball = {
				position = {x = 0.; y = 0.};
				speed = {x = 0.; y = 0.};
				accel = {x = 0.; y = 0.};
				links = [];
			};
			entities = [];
		};
	} in
	Graphics.open_graph (" "^(string_of_int (int_of_float (g.size.x)))^"x"^(string_of_int (int_of_float (g.size.y))));
	Graphics.auto_synchronize false;
	loop g
