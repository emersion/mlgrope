open Arg
open Unix

open Mlgrope
open Level
open Game
open Editor

let size = {x = 500.; y = 500.}

let play path =
	let ch = open_in path in
	let state = Level.input ch in
	let g = {
		size;
		time = Unix.gettimeofday ();
		paused = false;
		state;
	} in
	Game.run g;
	raise Exit

let edit path =
	let ch = open_in path in
	let gs = Level.input ch in
	Editor.run size gs;
	raise Exit

let () =
	try
		Arg.parse [
			("-edit", Arg.String(edit), "Edit a level")
		] play "Usage: mlgrope [-edit] level.csv";

		(* No level specified *)
		play "levels/0.csv"
	with Exit -> ()
