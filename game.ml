open Graphics

open Mlgrope
open Backend
open Frontend

exception OutOfBoundsException


let is_bubble_at pos ball e =
	match e with
	| Bubble(bubble) -> dist pos ball.position <= bubble.radius
	| _ -> false

let is_between a b x =
	(min a b) <= x && x <= (max a b)

let intersect_box_point a b pos =
	is_between a.x b.x pos.x && is_between a.y b.y pos.y

let line_equation a b =
	let k = (b.y -. a.y) /. (b.x -. a.x) in
	let y0 = a.y -. a.x *. k in
	let f x = k *. x +. y0 in
	(k, y0, f)

let intersect_line_point a b pos =
	let (k, y0, f) = line_equation a b in
	abs_float ((f pos.x) -. pos.y) <= 5.

let intersect_lines a b a' b' =
	(*
	k*x + y0 = k'*x + y0'
	x*(k - k') = y0' - y0
	x = (y0' - y0) / (k - k')
	*)
	if a == a' && b == b' then Some(a) else
	let (k, y0, f) = line_equation a b in
	let (k', y0', f') = line_equation a' b' in
	let x = (y0' -. y0) /. (k -. k') in
	let (y, y') = (f x, f' x) in
	if abs_float (y -. y') <= 1. then Some({x; y}) else None

let intesect_segment_point a b pos =
	intersect_box_point a b pos && intersect_line_point a b pos

let intersect_segments a b a' b' =
	match intersect_lines a b a' b' with
	| Some(pt) -> intersect_box_point a b pt && intersect_box_point a' b' pt
	| None -> false

let is_rope_at lastpos pos ball e =
	match e with
	| Rope(rope) -> intersect_segments rope.position ball.position lastpos pos
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
	if not (intersect_box_point {x = 0.; y = 0.} size b.position)
	then raise OutOfBoundsException

let step g =
	let t = Unix.gettimeofday () in
	let dt = t -. g.time in
	let g = { g with
		time = t;
		state = if g.paused then g.state else Backend.move g.state dt
	} in
	check_ball_bounds g.size g.state.ball;
	Graphics.clear_graph ();
	draw g.state;
	Graphics.synchronize ();
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
		let pos = {x = float_of_int mouse_x; y = float_of_int mouse_y} in
		let lastpos = match s with
		| {button = true; mouse_x; mouse_y} -> {x = float_of_int mouse_x; y = float_of_int mouse_y}
		| _ -> pos
		in
		{g with state = {g.state with ball = handle_click g.state.ball lastpos pos}}
	| {keypressed = true; key = '\027'} -> raise Exit
	| {keypressed = true; key = 'p'} -> {g with paused = not g.paused}
	| _ -> g

let run g =
	Frontend.run step handle_event g.size g
