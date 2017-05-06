open Arg

open Math2d
open Mlgrope
open Level
open Player
open Editor

let size = {x = 500.; y = 500.}

let play path =
	let ch = open_in path in
	let gs = Level.input ch in
	Player.run size gs;
	raise Exit

let edit path =
	Editor.run size path;
	raise Exit

let () =
	try
		Arg.parse [
			("-edit", Arg.String(edit), "Edit a level")
		] play "Usage: mlgrope [-edit] level.csv";

		(* No level specified *)
		play "levels/0.csv"
	with Exit -> ()
