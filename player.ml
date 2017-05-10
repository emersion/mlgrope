open Graphics

open Util
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
	state : game_state;
}

let star_score = 100

let pad_int padding n =
	let s = string_of_int n in
	let len = String.length s in
	let start_at = padding - len in
	if start_at < 0 then s else
	String.init padding (fun i ->
		if i >= start_at then String.get s (i - start_at) else '0'
	)

let draw_score size score =
	let (x, y) = ints_of_vec size in
	let s = pad_int 5 score in
	Graphics.set_color Graphics.black;
	let (w, h) = Graphics.text_size s in
	Graphics.moveto (x - w) (y - h);
	Graphics.draw_string s

let is_bubble_at pos ball e =
	match e with
	| Bubble(bubble) -> distance pos ball.position <= bubble.radius
	| _ -> false

let is_rope_at lastpos pos ball e =
	match e with
	| Rope(rope) ->
		is_some (Collide.segments rope.position ball.position lastpos pos)
	| _ -> false

let is_in_bounds size b =
	let (has_bubble, has_rope) = List.fold_left (fun (has_bubble, has_rope) e ->
		match e with
		| Bubble(_) -> (true, has_rope)
		| Rope(_) | Elastic(_) -> (has_bubble, true)
		| _ -> (has_bubble, has_rope)
	) (false, false) b.links in
	if has_bubble && b.position.y <= 0. then true else
	if has_rope then true else
	Collide.box_point vec0 size b.position

let compute_score gs =
	fold_balls (fun score ball ->
		List.fold_left (fun score e ->
			match e with
			| Star(_) -> score + star_score
			| _ -> score
		) 0 ball.links
	) 0 gs

let step g =
	let t = Unix.gettimeofday () in
	let dt = t -. g.time in
	let state = if g.paused then g.state else Backend.move g.state dt in
	let (state, in_bounds) = List.fold_left (fun (l, n) e ->
		match e with
		| Ball(b) ->
			if is_in_bounds g.size b then
				(e::l, n+1)
			else
				(l, n)
		| _ -> (e::l, n)
	) ([], 0) state in
	if in_bounds = 0 then raise OutOfBoundsException else
	let g = {g with time = t; state} in
	let score = compute_score g.state in
	Frontend.step (fun () ->
		Frontend.draw g.state;
		draw_score g.size score
	);
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
		let pos = Frontend.mouse_of_status s' in
		let lastpos = match s with
		| {button = true; mouse_x; mouse_y} -> {x = float_of_int mouse_x; y = float_of_int mouse_y}
		| _ -> pos
		in
		let state = List.map (fun e ->
			match e with
			| Ball(b) -> Ball(handle_click b lastpos pos)
			| _ -> e
		) g.state in
		{g with state}
	| {keypressed = true; key = '\027'} -> raise Exit
	| {keypressed = true; key = 'p'} -> {g with paused = not g.paused}
	| {keypressed = true; key = 'f'} ->
		let g = step {g with paused = false} in
		{g with paused = true}
	| _ -> g

let run size state =
	let g = {
		size;
		time = Unix.gettimeofday ();
		paused = false;
		state;
	} in
	Frontend.run step handle_event size g
