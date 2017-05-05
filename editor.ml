open Graphics

open Mlgrope
open Frontend

let step gs =
	Graphics.clear_graph ();
	draw gs;
	Graphics.synchronize ();
	gs

let handle_event gs s s' =
	match s' with
	| {keypressed = true; key = '\027'} -> raise Exit
	| _ -> gs

let run size gs =
	Frontend.run step handle_event size gs
