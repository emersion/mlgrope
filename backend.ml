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

let collision_cercle_point center radius p =
	squared_distance center p <= radius ** 2.

let collision_cercle_droite center radius p1 p2=
	let u = p2 -: p1 in
	let ac = center -: p1 in
	let d = (length ({x = u.x *. ac.y -. u.y *. ac.x; y = 0.})) /. (length u) in
	radius <= d

let collision_cercle_seg center radius p1 p2 =
	collision_cercle_droite center radius p1 p2
	&& ( dot (p2 -: p1) (center -: p1) >= 0. && dot (p1 -: p2) (center -: p2) >= 0.
			|| collision_cercle_point center radius p1
			|| collision_cercle_point center radius p2
			)


let elastic_force d (e : elastic) =
	(d/.250. *. e.stiffness)**2.0

let fold_balls f acc l =
	List.fold_left (fun acc e ->
		match e with
		| Ball(b) -> f acc b
		| _ -> acc
	) acc l

(* Col = colision list, entl = entities *)
let clear_entities col entl =
	List.fold_left (fun acc e ->
		match e with
		| Bubble(_) | Star(_) -> if (List.mem e col) then acc else e::acc
		| _ -> e::acc
		) [] entl

let check_collision pos ent =
	match ent with
	| Goal(g) -> let d = squared_distance g.position pos in
		if Mlgrope.ball_radius**2. >= d then raise TouchedGoalException else false
	| Star(s) -> let d = squared_distance s.position pos in
		Mlgrope.ball_radius**2. >= d
	| Bubble(bu) ->	let d = squared_distance bu.position pos in
		(Mlgrope.ball_radius +. bu.radius)**2. >= d
	| Rope(r) -> squared_distance r.position pos >= r.length**2.
	(* | Block(b) -> *)
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
		| Rope(r) -> 	if squared_distance r.position b.position <= r.radius**2.
										&& (not (List.mem e acc))
									then e::acc
									else acc
		| Elastic(el) -> if squared_distance el.position b.position <= el.radius**2.
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
		| Bubble(bu) -> ({gravity with y = 0.5*. abs_float gravity.y}::(fst acc), true)
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
		| Rope(r) -> if List.mem e linkList && distance pos r.position >= r.length
			then (projection (-1. *: sumForces) (r.position -: pos)) +: acc
			else acc
		| _ -> acc
	) sumForces colList

(* I need to move the ball where it can go *)
(* pos = previous pos, newPos position it would go to without any constraint *)
let apply_constraint b sumForces col =
	match col with
	| Rope(r) ->
		if distance b.position r.position >= r.length
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
				if List.mem e linkList && distance pos r.position >= r.length
				then r.length *: (direction r.position pos) +: r.position
				else pos
			| _ -> pos
	) pos colList

let sum_force l =
	List.fold_left (+:) vec0 l

let ball_move ball gs dt =
	let colList = List.filter (check_collision ball.position) gs in
	let forceList = compute_forces ball.position ball.links in
	let sumForces = sum_force forceList in
	let links = update_links colList ball.links in
	let links = link_entities gs {ball with links} in
	(* compute links impact on the forces / movement / position *)
	let sumForces = compute_reaction ball.position sumForces colList links in
	let speed = (apply_constraints ball sumForces colList links) +: dt *: sumForces in
	let position = compute_position (ball.position +: dt *: speed) colList links in
	let updated = {position; speed; links} in
	let gs = clear_entities colList gs in
	List.map (swap_ball ball updated) gs

let move gs dt =
	fold_balls (fun gs b -> ball_move b gs dt) gs gs
