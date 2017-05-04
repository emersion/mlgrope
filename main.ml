open Mlgrope
open Frontend

let () =
	let rope = {position = {x = 100.; y = 300.}; radius = 40.; length = 150.} in
	let g = {
		size = {x = 500.; y = 500.};
		time = Unix.gettimeofday ();
		paused = false;
		state = {
			ball = {
				position = {x = 200.; y = 400.};
				speed = {x = 0.; y = 0.};
				links = [Rope(rope)];
			};
			entities = [
				Bubble{position = {x = 200.; y = 100.}; radius = 50.};
				Rope(rope);
				Goal{position = {x = 200.; y = 450.}};
			];
		};
	} in
	Frontend.run g
