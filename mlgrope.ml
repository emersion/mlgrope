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

type game_state = entity list

let ball_radius = 10.0

let update_ball ball updated =
	fun e ->
		match e with
		| Ball(b) -> if b == ball then Ball(updated) else e
		| _ -> e
