open String
open Sys
open Graphics

open Math2d
open Collide
open Frontend
open Level
open Player

exception SelectLevel of string

type menu = {
	size : vec;
	levels : string array;
	selected : int;
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

let draw_level size l =
	Graphics.set_color Graphics.black;
	Graphics.moveto 0 0;
	Graphics.draw_string ("Level "^l)

let step m =
	Frontend.step (fun () ->
		draw_level m.size m.levels.(m.selected);
		draw_seek_button m.size (-1);
		draw_seek_button m.size 1
	);
	m

let handle_event m s s' =
	match s' with
	| {button = true} ->
		let pos = mouse_of_status s' in
		let delta =
			if collide_seek_button m.size (-1) pos then -1
			else if collide_seek_button m.size 1 pos then 1
			else (
				let path = levels_dir^"/"^m.levels.(m.selected)^levels_ext in
				raise (SelectLevel path)
			)
		in
		{m with selected = m.selected + delta}
	| {keypressed = true; key = '\027'} -> raise Exit
	| _ -> m

let run size =
	let files = Sys.readdir levels_dir in
	let levels = Array.map (strip_suffix levels_ext) files in
	Array.sort String.compare levels;

	let m = {size; levels; selected = 0} in
	try
		Frontend.run step handle_event size m
	with SelectLevel(path) ->
		let ch = open_in path in
		let gs = Level.input ch in
		Player.run size gs
