open Sys
open Graphics

open Math2d
open Collide
open Mlgrope
open Backend
open Frontend
open Level

let panel_width = 100.
let panel_color = Graphics.rgb 127 127 127

let grid_size = 10.
let grid_color = Graphics.rgb 230 230 230

type editor = {
	size : vec;
	state: game_state;
	selected : entity option;
}

let draw_grid size =
	let grid_size = int_of_float grid_size in
	let (w, h) = ints_of_vec size in
	Graphics.set_color grid_color;
	for i = 0 to w/grid_size do
		Graphics.moveto (i*grid_size) 0;
		Graphics.lineto (i*grid_size) h
	done;
	for i = 0 to h/grid_size do
		Graphics.moveto 0 (i*grid_size);
		Graphics.lineto w (i*grid_size)
	done

let panel_entities size =
	let n = 5 in
	let x = size.x +. (panel_width /. 2.) in
	let h = round_float (size.y /. (float_of_int (n+1))) in
	let radius = 20. in
	[
		Bubble{position = {x; y = h}; radius};
		Rope{position = {x; y = 2.*.h}; radius; length = radius};
		Elastic{position = {x; y = 3.*.h}; radius; length = radius; stiffness = 1.};
		Goal{position = {x; y = 4.*.h}};
		Star{position = {x; y = 5.*.h}};
		(* TODO: block *)
	]

let draw_panel size =
	let (w, h) = ints_of_vec size in
	Graphics.set_color panel_color;
	Graphics.fill_rect w 0 (w + int_of_float panel_width) h;

	let l = panel_entities size in
	List.iter Frontend.draw_entity l

let step ed =
	Frontend.step (fun () ->
		draw_grid ed.size;
		draw_panel ed.size;
		Frontend.draw ed.state;
	);
	ed

let stick_to_grid pt =
	Math2d.map (fun k -> grid_size *. round_float (k /. grid_size)) pt

let intersect_entity pt ent =
	match ent with
	| Ball{position} ->
		Mlgrope.ball_radius**2. >= squared_distance position pt
	| Goal{position} ->
		Frontend.goal_radius**2. >= squared_distance position pt
	| Star{position} ->
		Frontend.star_radius**2. >= squared_distance position pt
	| Bubble{position; radius} | Rope{position; radius} ->
		radius**2. >= squared_distance position pt
	| Block{vertices} -> Collide.polygon_point vertices pt
	| _ -> false

let intersect_ball pt (b : ball) =
	Mlgrope.ball_radius**2. >= squared_distance b.position pt

let intersect_object pt state =
	try
		Some(List.find (intersect_entity pt) state)
	with Not_found -> None

let update_entity_position e position =
	match e with
	| Ball(b) -> Ball{b with position}
	| Bubble(b) -> Bubble{b with position}
	| Rope(b) -> Rope{b with position}
	| Elastic(b) -> Elastic{b with position}
	| Goal(b) -> Goal{position}
	| Star(b) -> Star{position}
	| Block(b) ->
		let center = average b.vertices in
		let delta = position -: center in
		let vertices = List.map (fun v -> v +: delta) b.vertices in
		Block{b with vertices}

let update_position ed entity position =
	let updated = update_entity_position entity position in
	let state = List.map (fun e ->
		if e == entity then updated else e
	) ed.state in
	{ed with state; selected = Some(updated)}

let remove ed entity =
	match entity with
	| Ball(_) -> ed
	| _ ->
		let state = List.filter (fun e -> e != entity) ed.state in
		{ed with state; selected = None}

let handle_event path ed s s' =
	let pos = Frontend.vec_of_status s' in
	let outside = pos.x > ed.size.x in
	let update_position ed obj pos =
		let pos = if outside then pos else stick_to_grid pos in
		update_position ed obj pos
	in
	match (s, s') with
	| ({button = false}, {button = true}) -> (
		match intersect_object pos ed.state with
		| Some(obj) -> update_position ed obj pos
		| None -> (
			let l = panel_entities ed.size in
			try
				let e = List.find (intersect_entity pos) l in
				let ed = {ed with state = e::ed.state} in
				update_position ed e pos
			with Not_found -> ed
		)
	)
	| ({button = true}, {button}) -> (
		if not button && outside then
			match ed.selected with
			| Some(obj) -> remove ed obj
			| None -> ed
		else
			let ed = match ed.selected with
			| Some(obj) -> update_position ed obj pos
			| None -> ed
			in
			if button then ed else {ed with selected = None}
	)
	| (_, {keypressed = true; key = '\027'}) -> raise Exit
	| (_, {keypressed = true; key = 'w'}) ->
		let ch = open_out path in
		Level.output ch ed.state;
		close_out ch;
		Printf.printf "Saved %s\n%!" path;
		ed
	| _ -> ed

let run size path =
	let state =
		if Sys.file_exists path then
			let ch = open_in path in
			let state = Level.input ch in
			close_in ch;
			Printf.printf "Loaded %s\n%!" path;
			state
		else
			[]
	in

	Printf.printf "Press w to save\n%!";

	let ed = {
		size;
		state;
		selected = None;
	}
	in
	Frontend.run step (handle_event path) (size +: {x = panel_width; y = 0.}) ed
