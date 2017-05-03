open Mlgrope

exception OutOfBoundsException
exception TouchedGoalException

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

let distance v1 v2  =
	(v1.x -. v2.x)**2.  +. (v1.y -. v2.y)**2.

let dot v1 v2  =
	v1.x *. v2.x +. v1.y *. v2.y

let norm v =
	sqrt (v.x**2. +. v.y**2.)

(* v1 -> v2 -> v3 projects v1 on v2 according to y result is v *)
let projection v1 v2 =
	let co = (dot v1 v2) /. ((norm v1) *. (norm v2)) in
	let angle = acos co in
	{ x = tan angle *. v1.y; y = v1.y }

let remove_from_list o l = List.filter (fun e -> o != e) l

(* Col = colision list, entl = entities *)
let clear_entities col entl =
	List.fold_left (fun acc e ->
		match e with
		| Bubble(bu) -> if (List.mem e col) then acc else e::acc
		| _ -> e::acc
		) [] entl

(* Add links that needs to be added, col = collision list, l = link list *)
(* TODO : separate in Two fun (if GP contains spikes)
 		* add_links
		* remove_links
*)
let rec update_links col l =
	List.fold_left (fun acc e ->
		match e with
		| Bubble(bu) -> e::acc
		| _ -> acc
		) l col

let check_collision b ent =
	match ent with
	| Goal(g) ->
		let dist = distance g.position b.position in
		if Mlgrope.ball_radius**2. >= dist then raise TouchedGoalException else false
	| Bubble(bu) ->
		let dist = (bu.position.x -. b.position.x)**2.  +. (bu.position.y -. b.position.y)**2. in
		(Mlgrope.ball_radius +. bu.radius)**2. >= dist
	| _ -> false

let check_collisions b entl =
	List.fold_left (fun acc e -> if check_collision b e then e::acc else acc)
	[] entl


let link_entities entl b =
	List.fold_left (fun acc e ->
									match e with
									| Rope(r) -> 	if distance r.position b.position <= r.radius
																then { acc with links = e::acc.links }
																else acc
									| _ -> acc
								)	b entl

(* Link list : entity list -> Forces list : position list *)
(* Collision response *)
let compute_forces l =
	let (l,isBubble) = List.fold_left (fun acc e ->
		match e with
		| Bubble(bu) -> ({gravity with y = abs gravity.y}::(fst acc), true)
		| _ -> let (l,isBubble) = acc in
			if isBubble then (l, true) else (l, false)
		) ([],false) l in
	if isBubble then l else gravity::l
		(* TODO : ajouter la réaction du support *)

let sum_force l =
	List.fold_left (+) { x = 0.; y = 0. } l

let ball_move g dt =
	(* Compute new pos *)
	let b = g.ball in
	let ent = g.entities in
	let colList = check_collisions b g.entities in
	let forceList = compute_forces b.links in
	let sumForces = sum_force forceList in
	let newSpeed = b.speed + dt * sumForces in
	let b = { b with
		position = b.position + dt * newSpeed;
		speed  = newSpeed;
		links = update_links colList b.links
	} in
	(* Check for collision  & respond *)

	(* Respond to collsions *)

	let ent = clear_entities colList ent in
	(* let sumForces = ball_sum_force *)
	(* Update position & speed *)
	{ball = b; entities = ent}

let move g dt =
	let g = (ball_move g dt) in
	if g.ball.position.y <= 0. then raise OutOfBoundsException
	else g
