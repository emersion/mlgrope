type vec = {
	x : float;
	y : float;
}

val squared_dist : vec -> vec -> float
val dist : vec -> vec -> float

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

type entity =
	| Bubble of bubble
	| Rope of rope
	| Goal of goal
	| Elastic of elastic
	| Star of star

type ball = {
	position : vec;
	speed : vec;
	links : entity list;
}

val ball_radius : float

type game_state = {
	ball : ball;
	entities : entity list;
}

type game = {
	size : vec;
	time : float;
	paused : bool;
	state : game_state;
}
