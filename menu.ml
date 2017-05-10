open String
open Sys
open Graphics

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

let seek_button_size = 40.
let seek_button_color = Graphics.blue

let strip_suffix suffix s =
	let len = String.length s in
	let suffix_len = String.length suffix in
	if len < suffix_len then s else
	if String.sub s (len - suffix_len) suffix_len <> suffix then s else
	String.sub s 0 (len - suffix_len)

let seek_button_position size dir =
	let x = if dir < 0 then seek_button_size else size.x -. seek_button_size in
	{x; y = 0.5 *. size.y}

let collide_seek_button size dir pos =
	let btn_pos = seek_button_position size dir in
	let s = {x = seek_button_size; y = seek_button_size} in
	let corner = btn_pos -: (0.5 *: s) in
	Collide.box_point corner (corner +: s) pos

let draw_seek_button size dir =
	let (x, y) = ints_of_vec (seek_button_position size dir) in
	let s = int_of_float seek_button_size in
	Graphics.set_color seek_button_color;
	Graphics.fill_rect (x - s/2) (y - s/2) s s

let draw_level size name state =
	Frontend.draw state;
	Graphics.set_color Graphics.black;
	Graphics.moveto 0 0;
	Graphics.draw_string ("Level "^name)

let step m =
	draw_level m.size m.levels.(m.selected_index) m.selected_level;
	draw_seek_button m.size (-1);
	draw_seek_button m.size 1;
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
			if collide_seek_button m.size (-1) pos then -1
			else if collide_seek_button m.size 1 pos then 1
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
