open Math2d
open Mlgrope

exception TouchedGoalException

let print_forces l =
	List.iter (fun v -> Printf.printf "%f %f #\n%!" v.x v.y) l

(* gravity constant = -150 px.s-2 *)
let gravity = { x = 0.; y = -150. }

(* vecteur de norme 1 dans la direction v1v2 *)
let direction v1 v2 =
	normalize (v2 -: v1)

(* v1 -> v2 -> v projects v1 on v2 according to y result is v *)
let projection v1 v2 =
	(* a : la direction, co le cosinus *)
	let co = (dot v1 v2) /. ((Math2d.length v1) *. (Math2d.length v2)) in
	let a = { x = v2.x /. (Math2d.length v2); y = v2.y /. (Math2d.length v2) } in
	((Math2d.length v1) *. co) *: a

let elastic_force d (e : elastic) =
	(d/.250. *. e.stiffness)**2.0

let remove_from_list o l = List.filter (fun e -> o != e) l

let find_bubble l = List.exists (fun e -> match e with | Bubble(_) -> true	| _ -> false) l

let find_rope l = List.exists (fun e -> match e with | Rope(_) -> true	| _ -> false) l

(* Col = colision list, entl = entities *)
let clear_entities col entl =
	List.fold_left (fun acc e ->
		match e with
		| Bubble(_) | Star(_) -> if (List.mem e col) then acc else e::acc
		| _ -> e::acc
		) [] entl

let check_collision pos ent =
	let sqdRadius = Mlgrope.ball_radius *. Mlgrope.ball_radius in
	match ent with
	| Ball(b) -> let d = squared_distance b.position pos in
		(Mlgrope.ball_radius*.2.) *. (Mlgrope.ball_radius*.2.) >= d
	| Goal(g) -> let d = squared_distance g.position pos in
		if sqdRadius >= d then raise TouchedGoalException else false
	| Star(s) -> let d = squared_distance s.position pos in
		sqdRadius >= d
	| Bubble(bu) ->	let d = squared_distance bu.position pos in
		(Mlgrope.ball_radius +. bu.radius) *. (Mlgrope.ball_radius +. bu.radius) >= d
	| Rope(r) -> squared_distance r.position pos >= r.length *. r.length
	| Block(b) -> List.length (Collide.polygon_circle b.vertices pos Mlgrope.ball_radius) >= 1 (* TODO *)
	| _ -> false

(* Add links that needs to be added according to collisions *)
let rec add_links_collision col links =
	List.fold_left (fun acc e ->
		match e with
		| Bubble(_) | Star(_) -> e::acc
		| _ -> acc
	) links col

(* Add links tant needs to be added according to entity list *)
let add_links_entity pos entl links =
	List.fold_left (fun acc e ->
		match e with
		| Rope(r) -> 	if squared_distance r.position pos <= r.radius**2.
										&& (not (List.mem e acc))
									then e::acc
									else acc
		| Elastic(el) -> if squared_distance el.position pos <= el.radius**2.
										&& (not (List.mem e acc))
									then e::acc
									else acc
		| _ -> acc
	)	links entl

let add_links ball entl col =
	let links = add_links_collision col ball.links in
	add_links_entity ball.position entl links

(* ball pos, Link list : entity list -> Forces list : position list *)
(* Collision response *)
let compute_forces pos linkList =
	let (linkList,isBubble) = List.fold_left (fun acc e ->
		match e with
		| Bubble(bu) -> ({gravity with y = 0.5 *. abs_float gravity.y}::(fst acc), true)
		| Elastic(e) -> let d = distance pos e.position in
			if d >= e.length
			then ((elastic_force d e) *: direction pos e.position ::(fst acc), snd acc)
			else acc
		| _ -> acc
		) ([],false) linkList in
	if isBubble then linkList else gravity::linkList

let compute_reaction pos sumForces colList linkList =
	List.fold_left (fun acc e ->
		match e with
		| Ball(b) -> (projection sumForces (b.position -: pos)) +: acc
		| Rope(r) ->
			if List.mem e linkList && distance pos r.position >= r.length
			then (projection (-1. *: sumForces) (r.position -: pos)) +: acc
			else acc
		| Block(b) -> let l = Collide.polygon_circle b.vertices pos Mlgrope.ball_radius in
			List.fold_left (fun acc (a,b) ->
			let speed = (projection (sumForces) (b -: a -: pos)) in
				(projection (-1.*:sumForces) (b -: a)) +: acc
			) sumForces l
		| _ -> acc
	) sumForces colList

(* I need to move the ball where it can go *)
(* pos = previous pos, newPos position it would go to without any constraint *)
let apply_constraint ball sumForces col linkList =
	if sumForces =: Math2d.vec0 then ball.speed else
	match col with
	| Ball(b) -> projection ball.speed sumForces
	| Rope(r) ->
		if List.mem col linkList && distance ball.position r.position >= r.length
		then projection ball.speed sumForces
		else ball.speed
	| Block(bl) ->
		let l = Collide.polygon_circle bl.vertices ball.position Mlgrope.ball_radius in
		List.fold_left (fun acc (a,b) ->
			let vi = ball.speed in
			let n  = Collide.circle_seg_norm a b ball.position in
			let reflect = Math2d.reflect vi n in
			Math2d.reflect vi n
		) sumForces l
	| _ -> ball.speed

(* Apply constraints one after the other *)
let rec apply_constraints b sumForces colList linkList =
	match colList with
	| h::t -> let b = { b with speed = (apply_constraint b sumForces h linkList) } in
		apply_constraints b sumForces t linkList
	| [] -> b.speed

(* Handles collisions *)
let compute_position pos colList linkList =
	List.fold_left (fun acc e ->
			match e with
			| Ball(b) -> pos
			| Rope(r) ->
				if List.mem e linkList && distance pos r.position >= r.length
				then r.length *: (direction r.position pos) +: r.position
				else pos
			| _ -> pos
	) pos colList

let sum_vec l =
	List.fold_left (+:) { x = 0.; y = 0. } l

let filter_self ball gs  =
	List.filter (fun e ->
		match e with
		| Ball(b) -> not (ball = b)
		| _ -> true
	) gs

(* Compute new pos *)
let ball_move ball gs dt gs' =
	let gsNoBall = filter_self ball gs in
	let colList = List.filter (check_collision ball.position) gsNoBall in
	let forceList = compute_forces ball.position ball.links in
	let sumForces = sum_vec forceList in
	let links = add_links ball gsNoBall colList in
	(* compute links impact on the forces / movement / position *)
	let sumForces = compute_reaction ball.position sumForces colList links in
	let speed = (apply_constraints ball sumForces colList links) +: dt *: sumForces in
	let position = compute_position (ball.position +: dt *: speed) colList links in
	let updated = {position; speed; links} in
	let gs' = clear_entities colList gs' in
	List.map (swap_ball ball updated) gs'

let move gs dt =
	fold_balls (fun gs' b -> ball_move b gs dt gs') gs gs
