open String
open Graphics

open Mlgrope

exception Invalid_format

let vec0 = {x = 0.; y = 0.}

let parse_field f default l =
	match l with
	| h::t -> (f h, t)
	| [] -> (default, [])

let rec parse_field_list f l =
	match l with
	| [] -> []
	| _ ->
		let (v, l) = f l in
		v::(parse_field_list f l)

let parse_float l =
	parse_field float_of_string 0. l

let parse_vec l =
	let (x, l) = parse_float l in
	let (y, l) = parse_float l in
	({x; y}, l)

let parse_color l : (Graphics.color * string list) =
	let (i, l) = parse_field int_of_string 0 l in
	(i, l)

let parse_ball l : ball =
	let (position, l) = parse_vec l in
	{position; speed = vec0; links = []}

let parse_bubble l : bubble =
	let (position, l) = parse_vec l in
	let (radius, _) = parse_float l in
	{position; radius}

let parse_rope l : rope =
	let (position, l) = parse_vec l in
	let (radius, l) = parse_float l in
	let (length, _) = parse_float l in
	{position; radius; length}

let parse_elastic l : elastic =
	let (position, l) = parse_vec l in
	let (radius, l) = parse_float l in
	let (length, _) = parse_float l in
	let (stiffness, _) = parse_float l in
	{position; radius; length; stiffness}

let parse_goal l : goal =
	let (position, l) = parse_vec l in
	{position}

let parse_star l : star =
	let (position, l) = parse_vec l in
	{position}

let parse_block l : block =
	let (color, l) = parse_color l in
	let vertices = parse_field_list parse_vec l in
	{vertices; color}

let parse_entity t l =
	match t with
	| "bubble" -> Bubble(parse_bubble l)
	| "rope" -> Rope(parse_rope l)
	| "elastic" -> Elastic(parse_elastic l)
	| "goal" -> Goal(parse_goal l)
	| "star" -> Star(parse_star l)
	| "block" -> Block(parse_block l)
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
			let l = String.trim (input_line ch) in
			if String.length l == 0 then input gs else
			let l = String.split_on_char ',' l in
			let gs = parse_line l gs in
			input gs
		with End_of_file -> gs
	in
	try
		input gs
	with e ->
		close_in_noerr ch;
		raise e


let output_fields ch l =
	let l = String.concat "," l in
	output_string ch (l^"\n")

let cons_float f l =
	(string_of_float f)::l

let cons_vec v l =
	cons_float v.x (cons_float v.y l)

let cons_color (c : Graphics.color) l =
	("0x"^(Printf.sprintf "%X" c))::l

let fields_of_ball (b : ball) =
	cons_vec b.position []

let fields_of_bubble (b : bubble) =
	cons_vec b.position (cons_float b.radius [])

let fields_of_rope (r : rope) =
	cons_vec r.position (cons_float r.radius (cons_float r.length []))

let fields_of_elastic (e : elastic) =
	cons_vec e.position (cons_float e.radius (cons_float e.length (cons_float e.stiffness [])))

let fields_of_goal (g : goal) =
	cons_vec g.position []

let fields_of_star (s : star) =
	cons_vec s.position []

let fields_of_block (b : block) =
	cons_color b.color (List.fold_left (fun l v -> cons_vec v l) [] b.vertices)

let fields_of_entity e =
	match e with
	| Bubble(b) -> "bubble"::(fields_of_bubble b)
	| Rope(r) -> "rope"::(fields_of_rope r)
	| Elastic(e) -> "elastic"::(fields_of_elastic e)
	| Goal(g) -> "goal"::(fields_of_goal g)
	| Star(s) -> "star"::(fields_of_star s)
	| Block(b) -> "block"::(fields_of_block b)

let output ch gs =
	set_binary_mode_out ch true;
	output_fields ch ("ball"::(fields_of_ball gs.ball));
	output_char ch '\n';
	List.iter (fun e -> output_fields ch (fields_of_entity e)) gs.entities;
	flush ch
