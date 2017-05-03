open Graphics
open Sys
open Unix

open Mlgrope
open Backend

let tick_rate = 1. /. 60.

let ball_color = Graphics.black
let bubble_color = Graphics.red
let rope_color = Graphics.green
let goal_color = Graphics.blue

let dist a b =
	sqrt ((a.x -. b.x)**2. +. (a.y -. b.y)**2.)

let mix v1 v2 t =
	{ x = t *. v1.x +. (1. -. t) *. v2.x; y = t *. v1.y +. (1. -. t) *. v2.y }

let int_of_position p =
	(int_of_float p.x, int_of_float p.y)

let draw_bubble (b : bubble) =
	let (x, y) = int_of_position b.position in
	Graphics.set_color bubble_color;
	Graphics.draw_circle x y (int_of_float b.radius)

let draw_rope (r : rope) =
	let (x, y) = int_of_position r.position in
	Graphics.set_color rope_color;
	Graphics.draw_circle x y (int_of_float r.radius)

let draw_goal (g : goal) =
	let (x, y) = int_of_position g.position in
	Graphics.set_color goal_color;
	Graphics.fill_rect (x - 1) (y - 1) 2 2

let draw_ball (b : ball) =
	let (x, y) = int_of_position b.position in
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
	| Goal(g) -> draw_goal g

let rec draw_link b l =
	match l with
	| Rope({position}) ->
		let n = 10 in
		let line = Array.init (n+1) (fun i ->
			let t = (float_of_int i) /. (float_of_int n) in
			int_of_position (mix position b.position t)
			(* TODO *)
		) in
		Graphics.set_color Graphics.black;
		Graphics.draw_poly_line line
	| _ -> ()

let draw s =
	draw_ball s.ball;
	List.iter draw_entity s.entities;
	List.iter (draw_link s.ball) s.ball.links

let step g =
	let t = Unix.gettimeofday () in
	let dt = t -. g.time in
	let g = { g with time = t; state = Backend.move g.state dt } in
	Graphics.clear_graph ();
	draw g.state;
	Graphics.synchronize ();
	g

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

let handle_click ball lastpos pos =
	let is_bubble = is_bubble_at pos ball in
	let is_rope = is_rope_at lastpos pos ball in
	{ball with links = List.filter (fun e ->
		not (is_bubble e) && not (is_rope e)
	) ball.links}

let handle_event gs s s' =
	match s' with
	| {button = true; mouse_x; mouse_y} ->
		let pos = {x = float_of_int mouse_x; y = float_of_int mouse_y} in
		let lastpos = match s with
		| {button = true; mouse_x; mouse_y} -> {x = float_of_int mouse_x; y = float_of_int mouse_y}
		| _ -> pos
		in
		{gs with ball = handle_click gs.ball lastpos pos}
	| {keypressed = true; key = '\027'} -> raise Exit
	| _ -> gs

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
		g := {!g with state = handle_event !g.state !s s'};
		s := s'
	)
