open String
open Sys
open Graphics

open Image
open Math2d
open Collide
open Mlgrope
open Frontend
open Level
open Player

exception SelectLevel of game_state

type menu = {
	size : vec;
	levels : string array;
	selected_index : int;
	selected_level : game_state;
}

let levels_dir = "levels"
let levels_ext = ".csv"

let arrow_left_img = Image.get (Image.Ppm_file "img/arrow-left.ppm")
let arrow_right_img = Image.get (Image.Ppm_file "img/arrow-right.ppm")
let arrow_size = {x = 132.; y = 165.}

(* Removes suffix from s, if present *)
let strip_suffix suffix s =
	let len = String.length s in
	let suffix_len = String.length suffix in
	if len < suffix_len then s else
	if String.sub s (len - suffix_len) suffix_len <> suffix then s else
	String.sub s 0 (len - suffix_len)

let arrow_corner size dir =
	let x = if dir < 0 then -. 0.2 *. arrow_size.x else size.x -. 0.8 *. arrow_size.x in
	{x; y = 0.5 *. (size.y -. arrow_size.y)}

let collide_arrow size dir pos =
	let corner = arrow_corner size dir in
	Collide.box_point corner (corner +: arrow_size) pos

let draw_arrow size dir =
	let (x, y) = ints_of_vec (arrow_corner size dir) in
	Graphics.draw_image (if dir < 0 then arrow_left_img () else arrow_right_img ()) x y

let draw_level size name state =
	Frontend.draw state;
	Graphics.set_color Graphics.black;
	Graphics.moveto 0 0;
	Graphics.draw_string ("Level "^name)

let step m =
	draw_level m.size m.levels.(m.selected_index) m.selected_level;
	draw_arrow m.size (-1);
	draw_arrow m.size 1;
	m

let load_level name =
	let path = levels_dir^"/"^name^levels_ext in
	let ch = open_in path in
	Level.input ch

let handle_event m s s' =
	match s' with
	| {button = true} ->
		let pos = mouse_of_status s' in
		let delta =
			if collide_arrow m.size (-1) pos then -1
			else if collide_arrow m.size 1 pos then 1
			else raise (SelectLevel m.selected_level)
		in
		let selected_index = m.selected_index + delta in
		let selected_index =
			if selected_index < 0 then 0
			else if selected_index >= Array.length m.levels then (Array.length m.levels) - 1
			else selected_index
		in
		let selected_level = load_level m.levels.(selected_index) in
		{m with selected_index; selected_level}
	| {keypressed = true; key = '\027'} -> raise Exit
	| _ -> m

let run size =
	let files = Sys.readdir levels_dir in
	let levels = Array.map (strip_suffix levels_ext) files in
	Array.sort String.compare levels;
	if Array.length levels = 0 then raise (Failure "No level available") else

	let m = {size; levels; selected_index = 0; selected_level = load_level levels.(0)} in
	try
		Frontend.run step handle_event size m
	with SelectLevel(state) -> Player.run size state
