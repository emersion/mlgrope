open Mlgrope

let a  = {x = 3.0; y = 5.2}
let b  = {x = 4.2; y = 0.3}

let (+) v1 v2 =
	{ x = v1.x +. v2.x; y = v1.y +. v2.y }

let (=) v1 v2 =
	v1.x = v2.x && v1.y = v2.y 

let (-) v1 v2 =
	{ x = v1.x -. v2.x; y = v1.y -. v2.y }


let dot v1 v2  =  
	{ x = v1.x *. v2.x; y = v1.y *. v2.y }

let ( * ) s v  = 
	{ x = s *. v.x; y = s *. v.y }

let print_vector v =
	print_string ("x = "); print_float v.x;
	print_string " y = "; print_float v.y


let ball_move b dt =
	(* Compute new pos *)
	let nspeed = b.speed + dt * b.accel in
	let newB = { b with speed  = nspeed; position = b.position + nspeed } in

	(* Check for collision*)

	(* Respond to collision *)

	(* Update position & speed *)
	newB

let check_collision b ent =
		match ent with
		| Bubble(bu) ->
			let dist = (bu.position.x -. b.position.x)**2.0  +. (bu.position.y -. b.position.y)**2.0 in
			(Mlgrope.ball_radius +. bu.radius)**2.0 >= dist
		| _ -> false


let rec check_collisions b entl =
		match entl with
		| [] -> false
		| e::s -> (check_collision b e) || (check_collisions b s) 

let move g dt =
	{ g with ball = (ball_move g.ball dt) }