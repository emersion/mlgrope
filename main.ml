open Mlgrope
open Frontend

let () =
	let rope = {position = {x = 300.; y = 100.}; radius = 40.; length = 40.} in
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
	Frontend.run g
