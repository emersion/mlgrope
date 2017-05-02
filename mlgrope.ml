type vec = {
	x : float;
	y : float;
}

type bubble = {
	position : vec;
	radius : float;
}

type rope = {
	position : vec;
	radius : float;
}

type entity =
	| Bubble of bubble
	| Rope of rope

type ball = {
	position : vec;
	speed : vec;
	accel : vec;
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
	state : game_state;
}
