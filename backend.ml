open Math2d
open Mlgrope

exception TouchedGoalException of vec
exception DestroyBallException

let gravity = { x = 0.; y = -150. }

(* vecteur de norme 1 dans la direction v1v2 *)
let direction v1 v2 =
	normalize (v2 -: v1)

(* v1 -> v2 -> v projects v1 on v2 according to y result is v *)
let projection v1 v2 =
	(* a : la direction, co le cosinus *)
	let co = (dot v1 v2) /. ((Math2d.length v1) *. (Math2d.length v2)) in
	((Math2d.length v1) *. co) *: (normalize v2)

let spike_vertices (s : spike) =
	let v = s.position in
	let agl = s.angle in
	let s3 = sqrt 3. in
	let radius = Mlgrope.spike_edge_size *. s3 /. 4. in
	[
		v+: {x =  radius *. cos agl; y = radius *. sin agl};
		v+: {x =  radius *. cos (agl +. 2.*.Util.pi /. 3.); y = radius *. sin (agl +. 2.*.Util.pi /. 3.)};
		v+: {x =  radius *. cos (agl +. 4.*.Util.pi /. 3.); y = radius *. sin (agl +. 4.*.Util.pi /. 3.)};
	]

let elastic_force d (e : elastic) =
	(100. *. e.strength *. d /. ( e.radius *. e.radius))

let magnet_force d (m : magnet) =
	(200. *. m.strength *. m.radius *. m.radius /. d)

let remove_from_list o l =
	List.filter (fun e -> o != e) l

let find_bubble l =
	List.exists (fun e -> match e with | Bubble(_) -> true | _ -> false) l

let find_rope l =
	List.exists (fun e -> match e with | Rope(_) -> true | _ -> false) l

let clear_entities col entl =
	List.fold_left (fun acc e ->
		match e with
		| Bubble(_) | Star(_) ->
			if List.mem e col then acc else e::acc
		| _ -> e::acc
	) [] entl

let update_previous_links links prev_links =
	let prev_links = List.filter (fun e ->
		match e with
		| Fan(_) -> false
		| _ -> true
	) prev_links in
	List.fold_left (fun acc e ->
		if List.mem e acc then acc else e::acc
	) prev_links links

let check_collision pos ent =
	let sqdRadius = Mlgrope.ball_radius *. Mlgrope.ball_radius in
	match ent with
	| Ball(b) -> let d = squared_distance b.position pos in
		(Mlgrope.ball_radius*.2.) *. (Mlgrope.ball_radius*.2.) >= d
	| Goal(g) -> let d = squared_distance g.position pos in
		if sqdRadius >= d then raise (TouchedGoalException pos) else false
	| Star(s) -> let d = squared_distance s.position pos in
		sqdRadius >= d
	| Bubble(bu) ->	let d = squared_distance bu.position pos in
		(Mlgrope.ball_radius +. bu.radius) *. (Mlgrope.ball_radius +. bu.radius) >= d
	(* | Magnet(m) -> squared_distance m.position pos <= m.radius *. m.radius *)
	| Rope(r) -> squared_distance r.position pos >= r.length *. r.length
	| Block(b) -> List.length (Collide.polygon_circle b.vertices pos Mlgrope.ball_radius) >= 1
	(* | Fan(f) ->	 Kills you if you get in it or not ? *)
	| Spike(s) -> let vertices = spike_vertices s in
		if List.length (Collide.polygon_circle vertices pos Mlgrope.ball_radius) >= 1
		then raise DestroyBallException
		else false
	| _ -> false

(* Add links that needs to be added according to collisions *)
let rec add_links_collision col links =
	List.fold_left (fun acc e ->
		match e with
		| Bubble(_) | Star(_) -> e::acc
		| _ -> acc
	) links col

(* Add links tant needs to be added according to entity list *)
let add_links_entity pos entl links prev_links =
	List.fold_left (fun acc e ->
		if not (List.mem e acc) then
			match e with
			| Rope(r) ->
				if not (List.mem e prev_links) && squared_distance r.position pos <= r.radius**2.
				then e::acc
				else acc
			| Elastic(el) ->
				if not (List.mem e prev_links) && squared_distance el.position pos <= el.radius**2.
				then e::acc
				else acc
			| Fan(f) ->
				let p = rotate f.angle (pos -: f.position) in
				let a = vec0 -:  {x = 0.; y = f.size.y /. 2.} in
				let b = a +: f.size in
				if Collide.box_point a b p then e::acc else acc
		 	| Magnet(m) ->
				if squared_distance m.position pos <= m.radius *. m.radius
				then e::acc
				else acc
			| _ -> acc
		else acc
	) links entl

let clear_links links =
	List.filter (fun e ->
		match e with
		| Fan(_) | Magnet(_) -> false
		| _ -> true
	) links

let update_links ball entl col =
	let links = clear_links ball.links in
	let links = add_links_collision col links in
	add_links_entity ball.position entl links ball.previous_links

(* Link response -> force *)
let compute_forces pos linkList =
	let (forceList,isBubble) = List.fold_left (fun acc e ->
		match e with
		| Bubble(bu) -> ({gravity with y = 0.5 *. abs_float gravity.y}::(fst acc), true)
		| Magnet(m) -> let d = squared_distance m.position pos in
			let d  = max 10. (abs_float d) in
			((magnet_force d m) *: direction pos m.position ::(fst acc), snd acc)
		| Elastic(e) -> let sqd = squared_distance pos e.position in
			if sqd >= e.length *. e.length
			then ((elastic_force sqd e) *: direction pos e.position ::(fst acc), snd acc)
			else acc
		| Fan(f) -> ((200. *. f.strength ) *: {x = cos f.angle; y = sin f.angle}::(fst acc), snd acc)
		| _ -> acc
	) ([],false) linkList
	in
	if isBubble then forceList else gravity::forceList

(* Collsion response -> forces (reaction) *)
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
				(projection (-1. *: sumForces) (b -: a)) +: acc
			) sumForces l
		| _ -> acc
	) sumForces colList

(* Collision response -> speed *)
let apply_constraint ball sumForces col linkList =
	(* if sumForces =: Math2d.vec0 then ball.speed else *)
	match col with
	| Ball(b) ->
		let d = direction b.position ball.position in
		let p = projection (ball.speed -: b.speed) sumForces in
		length (0.5 *:p) *: d
	| Rope(r) ->
		if List.mem col linkList && distance ball.position r.position >= r.length
		then projection ball.speed sumForces
		else ball.speed
	| Block(bl) ->
		let l = Collide.polygon_circle bl.vertices ball.position Mlgrope.ball_radius in
		List.fold_left (fun acc (a,b) ->
			let n  = Collide.circle_seg_norm a b ball.position in
			Math2d.reflect ball.speed n
		) sumForces l
	| _ -> ball.speed

let rec apply_constraints b sumForces colList linkList =
	match colList with
	| h::t -> let b = { b with speed = (apply_constraint b sumForces h linkList) } in
		apply_constraints b sumForces t linkList
	| [] -> b.speed

(* Collsion response -> position *)
let compute_position pos colList linkList =
	List.fold_left (fun acc e ->
		match e with
		| Ball(b) ->
			let d  = direction b.position pos in
			b.position +: 2. *. Mlgrope.ball_radius *: d
		| Rope(r) ->
			if List.mem e linkList && distance pos r.position >= r.length
			then r.length *: (direction r.position pos) +: r.position
			else pos
		| Block(bl) -> let l = Collide.polygon_circle bl.vertices pos Mlgrope.ball_radius in
			begin
			try
			  let (a,b) =  List.hd l in
			let n  = Collide.circle_seg_norm a b pos in
			(2. *. Mlgrope.ball_radius *: n)
			with
				| Failure(_) -> pos
			end
		| _ -> pos
	) pos colList

let sum_vec l =
	List.fold_left (+:) vec0 l

let filter_self ball gs  =
	List.filter (fun e ->
		match e with
		| Ball(b) -> ball != b
		| _ -> true
	) gs

(* Compute new pos *)
let ball_move ball gs dt gs' =
	let gsNoBall = filter_self ball gs in
	let previous_links = update_previous_links ball.links ball.previous_links in
	let colList = List.filter (check_collision ball.position) gsNoBall in
	let forceList = compute_forces ball.position ball.links in
	let sumForces = sum_vec forceList in
	let links = update_links ball gsNoBall colList in
	let sumForces = compute_reaction ball.position sumForces colList links in
	let speed = (apply_constraints ball sumForces colList links) +: dt *: sumForces in
	let position = compute_position (ball.position +: dt *: speed) colList links in
	let updated = {position; speed; links; previous_links} in
	let gs' = clear_entities colList gs' in
	List.map (swap_ball ball updated) gs'

let move gs dt =
	fold_balls (fun gs' b ->
		try ball_move b gs dt gs'
		with DestroyBallException -> filter_self b gs'
	) gs gs
