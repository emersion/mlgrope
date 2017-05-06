open Graphics

open Math2d
open Collide
open Mlgrope
open Backend
open Frontend

exception OutOfBoundsException

type game = {
	size : vec;
	time : float;
	paused : bool;
	state : Mlgrope.game_state;
}

let is_bubble_at pos ball e =
	match e with
	| Bubble(bubble) -> distance pos ball.position <= bubble.radius
	| _ -> false

let is_rope_at lastpos pos ball e =
	match e with
	| Rope(rope) -> Collide.segments rope.position ball.position lastpos pos
	| _ -> false

let check_ball_bounds size b =
	let (has_bubble, has_rope) = List.fold_left (fun (has_bubble, has_rope) e ->
		match e with
		| Bubble(_) -> (true, has_rope)
		| Rope(_) | Elastic(_) -> (has_bubble, true)
		| _ -> (has_bubble, has_rope)
	) (false, false) b.links in
	if has_bubble && b.position.y <= 0. then () else
	if has_rope then () else
	if not (Collide.box_point {x = 0.; y = 0.} size b.position)
	then raise OutOfBoundsException

let step g =
	let t = Unix.gettimeofday () in
	let dt = t -. g.time in
	let g = { g with
		time = t;
		state = if g.paused then g.state else Backend.move g.state dt
	} in
	check_ball_bounds g.size g.state.ball;
	Frontend.step (fun () -> Frontend.draw g.state);
	g

let handle_click ball lastpos pos =
	let is_bubble = is_bubble_at pos ball in
	let is_rope = is_rope_at lastpos pos ball in
	{ball with links = List.filter (fun e ->
		not (is_bubble e) && not (is_rope e)
	) ball.links}

let handle_event g s s' =
	match s' with
	| {button = true; mouse_x; mouse_y} ->
		let pos = Frontend.vec_of_status s' in
		let lastpos = match s with
		| {button = true; mouse_x; mouse_y} -> {x = float_of_int mouse_x; y = float_of_int mouse_y}
		| _ -> pos
		in
		{g with state = {g.state with ball = handle_click g.state.ball lastpos pos}}
	| {keypressed = true; key = '\027'} -> raise Exit
	| {keypressed = true; key = 'p'} -> {g with paused = not g.paused}
	| _ -> g

let run size state =
	let g = {
		size;
		time = Unix.gettimeofday ();
		paused = false;
		state;
	} in
	Frontend.run step handle_event size g
