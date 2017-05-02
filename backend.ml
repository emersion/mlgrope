open Mlgrope

let a  = {x = 3.0; y = 5.2}
let b  = {x = 4.2; y = 0.3}

let (+) v1 v2 =
	{ x = v1.x +. v2.x; y = v1.y +. v2.y }

let print_vector v =
	print_string "x = "; print_float v.x;
	print_string " y = "; print_float v.y
;;

let ball_move b =
	(* Compute new pos *)
	let nspeed = b.speed + b.accel in
	{ b with speed  = nspeed; position = b.position + nspeed}

	(* Check for collision*)

	(* Respond to collision *)

	(* Update position & speed *)