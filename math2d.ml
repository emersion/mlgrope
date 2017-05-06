type vec = {
	x : float;
	y : float;
}

let squared_dist a b =
	(a.x -. b.x)**2. +. (a.y -. b.y)**2.

let dist a b =
	sqrt (squared_dist a b)

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

let norm v =
	sqrt (v.x**2. +. v.y**2.)

let normalize v =
	{x = v.x /. (norm v); y  = v.y /. (norm v)}
