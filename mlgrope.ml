type vec = {
	x : float;
	y : float;
}

let squared_dist a b =
	(a.x -. b.x)**2. +. (a.y -. b.y)**2.

let dist a b =
	sqrt (squared_dist a b)

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

type entity =
	| Bubble of bubble
	| Rope of rope
	| Goal of goal
	| Elastic of elastic

type ball = {
	position : vec;
	speed : vec;
	links : entity list;
}

let ball_radius = 10.0

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
