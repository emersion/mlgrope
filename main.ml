open Arg
open Unix

open Mlgrope
open Level
open Game
open Editor

let play path =
	let ch = open_in path in
	let state = Level.input ch in
	let g = {
		size = {x = 500.; y = 500.};
		time = Unix.gettimeofday ();
		paused = false;
		state;
	} in
	Game.run g

let edit path =
	let ch = open_in path in
	let state = Level.input ch in
	Editor.run state

let () =
	Arg.parse [
		("-edit", Arg.String(edit), "Edit a level")
	] play "Usage: mlgrope [-edit] level.csv";

	(* No level specified *)
	play "levels/0.csv"
