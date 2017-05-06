open Graphics

open Math2d
open Mlgrope
open Backend
open Frontend
open Level

type editor_object =
	| Entity of entity
	| Ball of ball

type editor = {
	size : vec;
	state: game_state;
	selected : editor_object option;
}

let step ed =
	Frontend.step (fun () -> Frontend.draw ed.state);
	ed

let intersect_entity pt ent =
	match ent with
	| Goal{position} ->
		Frontend.goal_radius**2. >= squared_distance position pt
	| Star{position} ->
		Frontend.star_radius**2. >= squared_distance position pt
	| Bubble{position; radius} | Rope{position; radius} ->
		radius**2. >= squared_distance position pt
	| _ -> false

let intersect_ball pt (b : ball) =
	Mlgrope.ball_radius**2. >= squared_distance b.position pt

let update_entity_position e position =
	match e with
	| Bubble(b) -> Bubble{b with position}
	| Rope(b) -> Rope{b with position}
	| Elastic(b) -> Elastic{b with position}
	| Goal(b) -> Goal{position}
	| Star(b) -> Star{position}
	| e -> e

let update_ball_position (b : ball) position =
	{b with position}

let handle_event path ed s s' =
	let pos = Frontend.vec_of_status s' in
	match (s, s') with
	| ({button = false}, {button = true}) -> (
		try
			let selected = List.find (intersect_entity pos) ed.state.entities in
			let updated = update_entity_position selected pos in
			let entities = List.map (fun e ->
				if e == selected then updated else e
			) ed.state.entities in
			{ed with state = {ed.state with entities}; selected = Some(Entity(updated))}
		with Not_found ->
			if intersect_ball pos ed.state.ball then
				let ball = update_ball_position ed.state.ball pos in
				{ed with state = {ed.state with ball}; selected = Some(Ball(ball))}
			else ed
	)
	| ({button = true}, {button}) -> (
		match ed.selected with
		| Some(Entity(selected)) ->
			let updated = update_entity_position selected pos in
			let entities = List.map (fun e ->
				if e == selected then updated else e
			) ed.state.entities in
			let selected = if button then Some(Entity(updated)) else None in
			{ed with state = {ed.state with entities}; selected}
		| Some(Ball(ball)) ->
			let ball = update_ball_position ball pos in
			let selected = if button then Some(Ball(ball)) else None in
			{ed with state = {ed.state with ball}; selected}
		| _ -> ed
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
	let ch = open_in path in
	let state = Level.input ch in
	close_in ch;

	let ed = {
		size;
		state;
		selected = None;
	}
	in
	Frontend.run step (handle_event path) size ed
