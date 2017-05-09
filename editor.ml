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

let handle_size = 10.
let handle_color = Graphics.black

type entity_property =
	| Position
	| Radius
	| Vertex of vec

type editor = {
	size : vec;
	state: game_state;
	selected : entity option;
	selected_property : entity_property;
}

let update_position entity position =
	match entity with
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
	let position = {x = size.x +. (panel_width /. 2.); y = 0.} in
	let radius = 20. in
	let l = [
		Ball{position; speed = vec0; links = []};
		Bubble{position; radius};
		Rope{position; radius; length = radius};
		Elastic{position; radius; length = radius; stiffness = 1.};
		Goal{position};
		Star{position};
		(* TODO: block *)
	] in
	let n = List.length l in
	let h = round_float (size.y /. (float_of_int (n+1))) in
	List.mapi (fun i e ->
		update_position e {position with y = (float_of_int (i+1)) *. h}
	) l

let draw_panel size =
	let (w, h) = ints_of_vec size in
	Graphics.set_color panel_color;
	Graphics.fill_rect w 0 (w + int_of_float panel_width) h;

	let l = panel_entities size in
	List.iter Frontend.draw_entity l

let handle_position e =
	match e with
	| Bubble{position; radius} | Rope{position; radius} | Elastic{position; radius} ->
		position +: {x = radius; y = 0.} (* TODO: draw handle in a corner *)
	| _ -> raise Not_found

let draw_radius_handle e =
	match e with
	| Bubble{position; radius} | Rope{position; radius} | Elastic{position; radius} ->
		let (x, y) = ints_of_vec position in
		let r = int_of_float radius in
		let (x, y) = (x + r, y) in (* TODO: draw handle in a corner *)
		let hs = int_of_float handle_size in
		Graphics.set_color handle_color;
		Graphics.fill_rect (x - hs/2) (y - hs/2) hs hs
	| _ -> ()

let step ed =
	Frontend.step (fun () ->
		draw_grid ed.size;
		draw_panel ed.size;
		Frontend.draw ed.state;
		List.iter draw_radius_handle ed.state
	);
	ed

let stick_to_grid pt =
	Math2d.map (fun k -> grid_size *. round_float (k /. grid_size)) pt

let intersect_entity pt entity =
	match entity with
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

let intersect_handle pt e =
	Collide.circle_point (handle_position e) (handle_size /. 2.) pt

let rec intersect_entities pt state =
	match state with
	| e::state -> (
		try
			if intersect_handle pt e then
				Some(e, Radius)
			else
				raise Not_found
		with Not_found ->
			if intersect_entity pt e then
				Some(e, Position)
			else
				intersect_entities pt state
	)
	| _ -> None

let swap_entity entity updated =
	fun e -> if e == entity then updated else e

let update_radius entity position =
	let radius = distance (position_of_entity entity) position in
	if radius < grid_size then entity else
	match entity with
	| Bubble(b) -> Bubble{b with radius}
	| Rope(r) -> Rope{r with radius}
	| Elastic(e) -> Elastic{e with radius}
	| _ -> entity

let update ed entity prop position =
	let ed = {ed with selected_property = prop} in
	let updated = match prop with
	| Position -> update_position entity position
	| Radius -> update_radius entity position
	| _ -> entity
	in
	let state = List.map (swap_entity entity updated) ed.state in
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
	let update ed e prop pos =
		let pos = if outside then pos else stick_to_grid pos in
		update ed e prop pos
	in
	match (s, s') with
	| ({button = false}, {button = true}) -> (
		match intersect_entities pos ed.state with
		| Some(e, prop) -> update ed e prop pos
		| None -> (
			let l = panel_entities ed.size in
			try
				let e = List.find (intersect_entity pos) l in
				let ed = {ed with state = e::ed.state} in
				update ed e Position pos
			with Not_found -> ed
		)
	)
	| ({button = true}, {button}) -> (
		if not button && outside then
			match ed.selected with
			| Some(e) -> remove ed e
			| None -> ed
		else
			let ed = match ed.selected with
			| Some(e) -> update ed e ed.selected_property pos
			| None -> ed
			in
			if button then ed else {ed with selected = None}
	)
	| (_, {keypressed = true; key = '\027'}) | (_, {keypressed = true; key = 'q'}) ->
		raise Exit
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
			[
				Ball{
					position = 0.5 *: size;
					speed = vec0;
					links = [];
				};
			]
	in

	Printf.printf "Press w to save\n%!";

	let ed = {
		size;
		state;
		selected = None;
		selected_property = Position;
	}
	in
	Frontend.run step (handle_event path) (size +: {x = panel_width; y = 0.}) ed
