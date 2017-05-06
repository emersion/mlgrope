open Graphics

open Math2d
open Mlgrope
open Backend
open Frontend

type editor = {
	size : vec;
	state: game_state;
	selected : bool;
}

let step ed =
	Frontend.step (fun () -> Frontend.draw ed.state);
	ed

let intersect_entity pt ent =
	match ent with
	| Goal{position} ->
		Frontend.goal_radius**2. >= squared_dist position pt
	| Star{position} ->
		Frontend.star_radius**2. >= squared_dist position pt
	| Bubble{position; radius} | Rope{position; radius} ->
		radius**2. >= squared_dist position pt
	| _ -> false

let intersect_ball pt (b : ball) =
	Mlgrope.ball_radius**2. >= squared_dist b.position pt

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
	| ({button = false}, {button = true}) ->
		(try
			let selected = List.find (intersect_entity pos) ed.state.entities in
			let entities = List.filter (fun e -> e != selected) ed.state.entities in
			let selected = update_entity_position selected pos in
			let entities = selected::entities in
			{ed with state = {ed.state with entities}; selected = true}
		with Not_found -> ed)
	| ({button = true}, {button}) ->
		if ed.selected then
			match ed.state.entities with
			| selected::entities ->
				let selected = update_entity_position selected pos in
				let entities = selected::entities in
				{ed with state = {ed.state with entities}; selected = button}
			| _ -> ed
		else ed
	| (_, {keypressed = true; key = '\027'}) -> raise Exit
	| _ -> ed

let run size state =
	let ed = {
		size;
		state;
		selected = false;
	}
	in
	Frontend.run step handle_event size ed
