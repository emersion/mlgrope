open Mlgrope
open Frontend

let step gs =
	Graphics.clear_graph ();
	draw gs;
	Graphics.synchronize ();
	gs

let handle_event gs s =
	gs

let run size gs =
	Frontend.run step handle_event size gs
