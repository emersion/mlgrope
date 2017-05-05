open Graphics

open Mlgrope
open Frontend

let step gs =
	Graphics.clear_graph ();
	Frontend.draw gs;
	Graphics.synchronize ();
	gs

let handle_event gs s s' =
	match (s, s') with
	| ({button = false}, {button = true; mouse_x; mouse_y}) -> gs (* TODO *)
	| ({button = true}, {button; mouse_x; mouse_y}) -> gs (* TODO *)
	| (_, {keypressed = true; key = '\027'}) -> raise Exit
	| _ -> gs

let run size gs =
	Frontend.run step handle_event size gs
