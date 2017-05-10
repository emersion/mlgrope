open Graphics

open Math2d

type bubble = {
	position : vec;
	radius : float;
}

type rope = {
	position : vec;
	radius : float;
	length : float;
}

type elastic = {
	position : vec;
	radius : float;
	length : float;
	stiffness : float;
}

type goal = {
	position : vec;
}

type star = {
	position : vec;
}

type block = {
	vertices : vec list;
	color : Graphics.color;
}

type fan = {
	position : vec;
	size : vec;
	angle : float;
	strength : float;
}

type ball = {
	position : vec;
	speed : vec;
	links : entity list;
}
and entity =
	| Ball of ball
	| Bubble of bubble
	| Rope of rope
	| Goal of goal
	| Elastic of elastic
	| Star of star
	| Block of block
	| Fan of fan

type game_state = entity list

let ball_radius = 20.0

let position_of_entity e =
	match e with
	| Ball{position} | Bubble{position} | Rope{position} | Goal{position}
	| Elastic{position} | Star{position} | Fan{position} ->
		position
	| Block{vertices} -> average vertices

let swap_ball ball updated =
	fun e ->
		match e with
		| Ball(b) -> if b == ball then Ball(updated) else e
		| _ -> e

let fold_balls f acc l =
	List.fold_left (fun acc e ->
		match e with
		| Ball(b) -> f acc b
		| _ -> acc
	) acc l
