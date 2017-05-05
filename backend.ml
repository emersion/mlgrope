open Mlgrope

exception TouchedGoalException

let print_forces l =
	List.iter (fun v -> Printf.printf "%f %f #\n%!" v.x v.y) l

(* gravity constant = -150 px.s-2 *)
let gravity = { x = 0.; y = -150. }

let (+) v1 v2 =
	{ x = v1.x +. v2.x; y = v1.y +. v2.y }

let (=) v1 v2 =
	v1.x = v2.x && v1.y = v2.y

let (-) v1 v2 =
	{ x = v1.x -. v2.x; y = v1.y -. v2.y }

let ( * ) s v  =
	{ x = s *. v.x; y = s *. v.y }

let abs x = if x >= 0.0 then x else -. x

let dot v1 v2  =
	v1.x *. v2.x +. v1.y *. v2.y

let norm v =
	sqrt (v.x**2. +. v.y**2.)

let normalize v =
	{x = v.x /. (norm v); y  = v.y /. (norm v)}

(* vecteur de norme 1 dans la direction v1v2 *)
let direction v1 v2 =
	normalize (v2 - v1)

(* v1 -> v2 -> v projects v1 on v2 according to y result is v *)
let projection v1 v2 =
	(* a : la direction, co le cosinus *)
	let co = (dot v1 v2) /. ((norm v1) *. (norm v2)) in
	let a = { x = v2.x /. (norm v2); y = v2.y /. (norm v2) } in
	((norm v1) *. co) * a

let elastic_force d (e : elastic) =
	(d/.250. *. e.stiffness)**2.0

let remove_from_list o l = List.filter (fun e -> o != e) l

let find_bubble l = List.exists (fun e -> match e with | Bubble(bu) -> true	| _ -> false) l

let find_rope l = List.exists (fun e -> match e with | Rope(r) -> true	| _ -> false) l

(* Col = colision list, entl = entities *)
let clear_entities col entl =
	List.fold_left (fun acc e ->
		match e with
		| Star(s) -> if (List.mem e col) then acc else e::acc
		| Bubble(bu) -> if (List.mem e col) then acc else e::acc
		| _ -> e::acc
		) [] entl

let check_collision pos ent =
	match ent with
	| Goal(g) -> let d = squared_dist g.position pos in
		if Mlgrope.ball_radius**2. >= d then raise TouchedGoalException else false
	| Star(s) -> let d = squared_dist s.position pos in
		Mlgrope.ball_radius**2. >= d
	| Bubble(bu) ->
		let d = squared_dist bu.position pos in
		(Mlgrope.ball_radius +. bu.radius)**2. >= d
	| Rope(r) ->
		dist r.position pos >= r.length
	| _ -> false

(* Add links that needs to be added according to collisions *)
let rec update_links col links =
	List.fold_left (fun acc e ->
		match e with
		| Bubble(bu) -> e::acc
		| _ -> acc
		) links col

(* Add links tant needs to be added according to entity list *)
let link_entities entl b =
	List.fold_left (fun acc e ->
									match e with
									| Rope(r) -> 	if dist r.position b.position <= r.radius
																	&& (not (List.mem e acc))
																then e::acc
																else acc
									| _ -> acc
								)	b.links entl

(* ball pos, Link list : entity list -> Forces list : position list *)
(* Collision response *)
let compute_forces pos linkList =
	let (linkList,isBubble) = List.fold_left (fun acc e ->
		match e with
		| Bubble(bu) -> ({gravity with y = 0.5*. abs gravity.y}::(fst acc), true)
		| Elastic(e) -> let d = dist pos e.position in
			if d >= e.length
			then ((elastic_force d e) * direction pos e.position ::(fst acc), snd acc)
			else acc
		| _ -> acc
		) ([],false) linkList in
	if isBubble then linkList else gravity::linkList

let compute_reaction pos sumForces colList linkList =
	List.fold_left (fun acc e ->
		match e with
		| Rope(r) -> if List.mem e linkList && dist pos r.position >= r.length
			then (projection (-1. * sumForces) (r.position - pos)) + acc
			else acc
		| _ -> acc
	) sumForces colList

(* I need to move the ball where it can go *)
(* pos = previous pos, newPos position it would go to without any constraint *)
let apply_constraint b sumForces col =
	match col with
	| Rope(r) ->
		if dist b.position r.position >= r.length
		then projection b.speed sumForces
		else b.speed
	| _ -> b.speed

(* Apply constraints one after the other *)
let rec apply_constraints b sumForces colList linkList =
	match colList with
	| h::t -> if List.mem h linkList
		then let b = { b with speed = (apply_constraint b sumForces h) } in
			apply_constraints b sumForces t linkList
		else apply_constraints b sumForces t linkList
	| [] -> b.speed

(* Handles collisions *)
let compute_position pos colList linkList =
	List.fold_left (fun acc e ->
			match e with
			| Rope(r) ->
				if List.mem e linkList && dist pos r.position >= r.length
				then r.length * (direction r.position pos) + r.position
				else pos
			| _ -> pos
	) pos colList

let sum_force l =
	List.fold_left (+) { x = 0.; y = 0. } l

let ball_move g dt =
	(* Compute new pos *)
	let b = g.ball in
	let ent = g.entities in
	let colList = List.filter (check_collision b.position) ent in
	let forceList = compute_forces b.position b.links in
	let sumForces = sum_force forceList in
	let newLinks = update_links colList b.links in
	let newLinks = link_entities ent {b with links = newLinks} in
	(* compute links impact on the forces / movement / position *)
	let sumForces = compute_reaction b.position sumForces colList newLinks in
	let newSpeed = (apply_constraints b sumForces colList newLinks) + dt * sumForces in
	let newPos = compute_position (b.position + dt * newSpeed) colList newLinks in
	let newB = {
		position = newPos;
		speed = newSpeed;
		links = newLinks;
	} in
	let ent = clear_entities colList ent in
	{ball = newB; entities = ent}

let move g dt =
	let g = (ball_move g dt) in
	g
