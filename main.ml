open Arg

open Math2d
open Mlgrope
open Level
open Menu
open Editor

let size = {x = 800.; y = 800.}

let play path =
	let ch = open_in path in
	let gs = Level.input ch in
	Player.run size gs;
	raise Exit

let edit path =
	Editor.run size path;
	raise Exit

let menu () =
	Menu.run size;
	raise Exit

let () =
	try
		Arg.parse [
			("-play", Arg.String(play), "Play a level");
			("-edit", Arg.String(edit), "Edit a level");
		] play "Usage: mlgrope [-play|-edit] level.csv";

		(* No level specified *)
		menu ()
	with Exit -> ()
