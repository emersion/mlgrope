open Graphics

open Math2d
open Mlgrope
open Backend
open Frontend

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

let handle_event ed s s' =
	let pos = Frontend.vec_of_status s' in
	match (s, s') with
	| ({button = false}, {button = true}) -> (
		try
			let selected = List.find (intersect_entity pos) ed.state.entities in
			let entities = List.filter (fun e -> e != selected) ed.state.entities in
			let selected = update_entity_position selected pos in
			let entities = selected::entities in
			{ed with state = {ed.state with entities}; selected = Some(Entity(selected))}
		with Not_found ->
			if intersect_ball pos ed.state.ball then
				let ball = {ed.state.ball with position = pos} in
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
			let ball = {ball with position = pos} in
			{ed with state = {ed.state with ball}; selected = Some(Ball(ball))}
		| _ -> ed
	)
	| (_, {keypressed = true; key = '\027'}) -> raise Exit
	| _ -> ed

let run size state =
	let ed = {
		size;
		state;
		selected = None;
	}
	in
	Frontend.run step handle_event size ed
