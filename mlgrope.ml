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

type entity =
	| Bubble of bubble
	| Rope of rope
	| Goal of goal
	| Elastic of elastic
	| Star of star
	| Block of block

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
