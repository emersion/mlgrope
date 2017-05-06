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

let grid_size = 10
let grid_color = Graphics.rgb 230 230 230

type editor_object =
	| Entity of entity
	| Ball of ball

type editor = {
	size : vec;
	state: game_state;
	selected : editor_object option;
}

let draw_grid size =
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

let draw_panel size =
	let (w, h) = ints_of_vec size in
	Graphics.set_color panel_color;
	Graphics.fill_rect w 0 (w + int_of_float panel_width) h

let step ed =
	Frontend.step (fun () ->
		draw_grid ed.size;
		draw_panel ed.size;
		Frontend.draw ed.state;
	);
	ed

let intersect_entity pt ent =
	match ent with
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
		Some(Entity(List.find (intersect_entity pt) state.entities))
	with Not_found ->
		if intersect_ball pt state.ball then
			Some(Ball(state.ball))
		else None

let update_entity_position e position =
	match e with
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

let update_position ed obj position =
	match obj with
	| Entity(entity) ->
		let updated = update_entity_position entity position in
		let entities = List.map (fun e ->
			if e == entity then updated else e
		) ed.state.entities in
		{ed with state = {ed.state with entities}; selected = Some(Entity(updated))}
	| Ball(ball) ->
		let ball = {ball with position} in
		{ed with state = {ed.state with ball}; selected = Some(Ball(ball))}

let remove ed obj =
	let entities = match obj with
	| Entity(entity) -> List.filter (fun e -> e != entity) ed.state.entities
	| _ -> ed.state.entities
	in
	{ed with state = {ed.state with entities}; selected = None}

let handle_event path ed s s' =
	let pos = Frontend.vec_of_status s' in
	match (s, s') with
	| ({button = false}, {button = true}) -> (
		match intersect_object pos ed.state with
		| Some(obj) -> update_position ed obj pos
		| None -> ed
	)
	| ({button = true}, {button}) -> (
		if not button && pos.x > ed.size.x then
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
			{
				ball = {
					position = 0.5 *: size;
					speed = vec0;
					links = [];
				};
				entities = [];
			}
	in

	Printf.printf "Press w to save\n%!";

	let ed = {
		size;
		state;
		selected = None;
	}
	in
	Frontend.run step (handle_event path) (size +: {x = panel_width; y = 0.}) ed
