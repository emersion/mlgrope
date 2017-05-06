let is_between a b x =
	(min a b) <= x && x <= (max a b)

type vec = {
	x : float;
	y : float;
}

let vec0 = {x = 0.; y = 0.}

let squared_distance a b =
	(a.x -. b.x)**2. +. (a.y -. b.y)**2.

let distance a b =
	sqrt (squared_distance a b)

let (+:) v1 v2 =
	{ x = v1.x +. v2.x; y = v1.y +. v2.y }

let (=:) v1 v2 =
	v1.x = v2.x && v1.y = v2.y

let (-:) v1 v2 =
	{ x = v1.x -. v2.x; y = v1.y -. v2.y }

let ( *:) s v  =
	{ x = s *. v.x; y = s *. v.y }

let dot v1 v2  =
	v1.x *. v2.x +. v1.y *. v2.y

let length v =
	sqrt (v.x**2. +. v.y**2.)

let normalize v =
	{x = v.x /. (length v); y  = v.y /. (length v)}
