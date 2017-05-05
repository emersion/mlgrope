open Arg
open Unix

open Mlgrope
open Frontend
open Level

let play path =
	let ch = open_in path in
	let state = Level.input ch in
	let g = {
		size = {x = 500.; y = 500.};
		time = Unix.gettimeofday ();
		paused = false;
		state;
	} in
	Frontend.run g

let edit path =
	let ch = open_in path in
	let state = Level.input ch in
	(* TODO *)
	raise Exit

let () =
	Arg.parse [
		("-edit", Arg.String(edit), "Edit a level")
	] play "Usage: mlgrope [-edit] level.csv";

	(* No level specified *)
	play "levels/0.csv"
