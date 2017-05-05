open Unix

open Mlgrope
open Frontend
open Level

let () =
	let ch = open_in "levels/0.csv" in
	let state = Level.input ch in
	let g = {
		size = {x = 500.; y = 500.};
		time = Unix.gettimeofday ();
		paused = false;
		state;
	} in
	Frontend.run g
