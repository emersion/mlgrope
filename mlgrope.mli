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

val ball_radius : float

val position_of_entity : entity -> vec
val swap_ball : ball -> ball -> (entity -> entity)
val fold_balls : ('a -> ball -> 'a) -> 'a -> entity list -> 'a
