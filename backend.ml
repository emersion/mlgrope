open Mlgrope

exception OutOfBoundsException
exception TouchedGoalException
exception CollisionException of entity

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

let abs x = if x >= 0.0 then x else -. x

let print_vector v =
	print_string ("x = "); print_float v.x;
	print_string " y = "; print_float v.y

let check_collision b ent =
		match ent with
		| Goal(g) -> 
			let dist = (g.position.x -. b.position.x)**2.  +. (g.position.y -. b.position.y)**2. in
			if Mlgrope.ball_radius**2.0 >= dist
			then raise (CollisionException (Goal g))
		| Bubble(bu) ->
			let dist = (bu.position.x -. b.position.x)**2.  +. (bu.position.y -. b.position.y)**2. in
			if (Mlgrope.ball_radius +. bu.radius)**2. >= dist
			then raise (CollisionException (Bubble bu))
		| _ -> ()

let rec check_collisions b entl =
		match entl with
		| [] -> ()
		| e::s -> (check_collision b e) ; (check_collisions b s) 

let ball_move g dt =
	(* Compute new pos *)
	let b = g.ball in
	let newSpeed = b.speed + dt * b.accel in
	let newB = { b with speed  = newSpeed; position = b.position + dt * newSpeed } in

	(* Check for collision *)
	let newB = try check_collisions b g.entities; newB with
		| CollisionException (Goal go) -> raise TouchedGoalException
		| CollisionException (Bubble bu) ->
			let newAcc = { x = newB.accel.x; y = abs(newB.accel.y) } in
			{ newB with accel = newAcc }
	in
	(* Respond to collision *)

	(* Update position & speed *)
	newB 


let move g dt =
	let b = (ball_move g dt) in
	if b.position.y <= 0. then raise OutOfBoundsException
	else { g with ball = b }