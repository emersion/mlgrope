let round_float x = snd (modf (x +. copysign 0.5 x))

let is_between a b x =
	(min a b) <= x && x <= (max a b)

type vec = {
	x : float;
	y : float;
}

let vec0 = {x = 0.; y = 0.}

let ints_of_vec p =
	(int_of_float p.x, int_of_float p.y)

let map f v =
	{x = f v.x; y = f v.y}

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

let fold_segments f acc l =
	match l with
	| h::t ->
		let (_, acc) = List.fold_left (fun (last, acc) v ->
			(v, f acc (last, v))
		) (h, acc) t in
		acc
	| [] -> acc

let average l =
	let (sum, n) = List.fold_left (fun (sum, n) v -> (sum +: v, n+1)) (vec0, 0) l in
	(1. /. float_of_int n) *: sum
