open Str

open Mlgrope

exception Invalid_format

let vec0 = {x = 0.; y = 0.}

let parse_field f default l =
	match l with
	| h::t -> (f h, t)
	| [] -> (default, [])

let parse_float l =
	parse_field float_of_string 0. l

let parse_vec l =
	let (x, l) = parse_float l in
	let (y, l) = parse_float l in
	({x; y}, l)

let parse_ball l =
	let (position, l) = parse_vec l in
	{position; speed = vec0; links = []}

let parse_bubble l =
	let (position, l) = parse_vec l in
	let (radius, _) = parse_float l in
	{position; radius}

let parse_rope l =
	let (position, l) = parse_vec l in
	let (radius, l) = parse_float l in
	let (length, _) = parse_float l in
	{position; radius; length}

let parse_elastic l =
	let (position, l) = parse_vec l in
	let (radius, l) = parse_float l in
	let (length, _) = parse_float l in
	let (stiffness, _) = parse_float l in
	{position; radius; length; stiffness}

let parse_goal l =
	let (position, l) = parse_vec l in
	{position}

let parse_entity t l =
	match t with
	| "bubble" -> Bubble (parse_bubble l)
	| "rope" -> Rope (parse_rope l)
	| "elastic" -> Elastic (parse_elastic l)
	| "goal" -> Goal (parse_goal l)
	| _ -> raise Invalid_format

let parse_line l gs =
	match l with
	| "ball"::l -> {gs with ball = parse_ball l}
	| t::l -> {gs with entities = (parse_entity t l)::gs.entities}
	| _ -> gs

let input ch =
	(* Default values *)
	let vec0 = {x = 0.; y = 0.} in
	let ball = {position = vec0; speed = vec0; links = []} in
	let gs = {ball; entities = []} in

	let rec input gs =
		try
			let l = input_line ch in
			let l = Str.split (Str.regexp ",") l in
			let gs = parse_line l gs in
			input gs
		with End_of_file -> gs
	in
	try
		input gs
	with e ->
		close_in_noerr ch;
		raise e
