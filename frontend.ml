open Graphics
open Sys
open Unix

open Mlgrope
open Backend

exception OutOfBoundsException

let tick_rate = 1. /. 60.

let ball_color = Graphics.black
let bubble_color = Graphics.red
let rope_color = Graphics.green
let goal_color = Graphics.blue
let star_color = Graphics.yellow

let goal_size = 10
let star_size = 10

let dist = Mlgrope.dist

let mix a b t =
	t *. a +. (1. -. t) *. b

let mix_vec v1 v2 t =
	{ x = mix v1.x v2.x t; y = mix v1.y v2.y t }

let ints_of_vec p =
	(int_of_float p.x, int_of_float p.y)


let draw_bubble (b : bubble) =
	let (x, y) = ints_of_vec b.position in
	Graphics.set_color bubble_color;
	Graphics.draw_circle x y (int_of_float b.radius)

let draw_rope (r : rope) =
	let (x, y) = ints_of_vec r.position in
	Graphics.set_color rope_color;
	Graphics.draw_circle x y (int_of_float r.radius)

let draw_elastic (e : elastic) =
	let (x, y) = ints_of_vec e.position in
	Graphics.set_color rope_color;
	Graphics.draw_circle x y (int_of_float e.radius)

let draw_goal (g : goal) =
	let (x, y) = ints_of_vec g.position in
	Graphics.set_color goal_color;
	Graphics.fill_rect (x - (goal_size/2)) (y - (goal_size/2)) goal_size goal_size

let draw_star (s : star) =
	let (x, y) = ints_of_vec s.position in
	Graphics.set_color star_color;
	Graphics.fill_rect (x - (star_size/2)) (y - (star_size/2)) star_size star_size

let draw_block (b : block) =
	let l = List.map ints_of_vec b.vertices in
	Graphics.set_color b.color;
	Graphics.fill_poly (Array.of_list l)

let draw_ball (b : ball) =
	let (x, y) = ints_of_vec b.position in
	Graphics.set_color ball_color;
	Graphics.fill_circle x y (int_of_float Mlgrope.ball_radius);

	List.iter (fun e ->
		match e with
		| Bubble(bubble) -> draw_bubble {bubble with position = b.position}
		| _ -> ()
	) b.links

let draw_entity e =
	match e with
	| Bubble(b) -> draw_bubble b
	| Rope(r) -> draw_rope r
	| Elastic(e) -> draw_elastic e
	| Goal(g) -> draw_goal g
	| Star(s) -> draw_star s
	| Block(b) -> draw_block b

(* Newton's method *)
let find_zero f x0 =
	let accuracy = 10.**(-4.) in
	let epsilon = 10.**(-10.) in
	let maxiter = 50 in
	let dx = 10.**(-4.) in
	let f' x = ((f x) -. (f (x -. dx))) /. dx in
	let rec find_zero n x =
		if n > maxiter then raise Not_found else
		let (y, y') = (f x, f' x) in
		if abs_float y' < epsilon then raise Not_found else
		let x' = x -. y /. y' in
		if abs_float (x' -. x) <= accuracy *. abs_float x' then
			x'
		else
			find_zero (n+1) x'
	in
	find_zero 0 x0

let build_poly_line f =
	let n = 100 in
	Array.init (n+1) (fun i ->
		let t = (float_of_int i) /. (float_of_int n) in
		ints_of_vec (f t)
	)

let create_line a b =
	fun t -> mix_vec a b t

let create_rope_line a b l =
	let (a, b) = if a.x < b.x then (a, b) else (b, a) in
	let (x1, y1) = (a.x, a.y) in
	let (x2, y2) = (b.x, b.y) in
	let c1 = (y2 -. y1) /. l in
	let c2 = copysign (sqrt ((c1**2.) /. (1. -. c1**2.))) c1 in
	let f a = y2 -. y1 -. 2. *. a *. c2 *. (sinh ((x2 -. x1) /. (2. *. a))) in
	(* Printf.printf "%f %f %f %f %f\n%!" x1 y1 x2 y2 (f (-10.)); *)
	let a = find_zero f ((x2 -. x1) /. 2.) in
	let h x0 = y2 -. y1 -. a *. ((cosh ((x2 -. x0) /. a)) -. (cosh ((x1 -. x0) /. a))) in
	let x0 = find_zero h 0. in
	let y0 = y1 -. a *. (cosh ((x1 -. x0) /. a)) +. a in
	let y x = a *. (cosh ((x -. x0) /. a)) +. (y0 -. a) in
	fun t -> let x = mix x1 x2 t in {x; y = y x}

let draw_link b l =
	match l with
	| Rope{position; length} | Elastic{position; length} ->
		let line = build_poly_line (
			try create_rope_line position b.position length
			with _ -> create_line position b.position
		) in
		Graphics.set_color Graphics.black;
		Graphics.draw_poly_line line
	| _ -> ()

let draw s =
	draw_ball s.ball;
	List.iter draw_entity s.entities;
	List.iter (draw_link s.ball) s.ball.links

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
	let (w, h) = (int_of_float g.size.x, int_of_float g.size.y) in
	Graphics.open_graph (" "^(string_of_int w)^"x"^(string_of_int h));
	Graphics.auto_synchronize false;

	let g = ref (step g) in
	let s = ref {mouse_x = 0; mouse_y = 0; button = false; keypressed = false; key = '\000'} in
	Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> g := step !g));
	let _ = Unix.setitimer Unix.ITIMER_REAL {it_interval = tick_rate; it_value = tick_rate} in
	let events = [Graphics.Button_down; Graphics.Mouse_motion; Graphics.Key_pressed] in
	Graphics.loop_at_exit events (fun s' ->
		g := handle_event !g !s s';
		s := s'
	)
