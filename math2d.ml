open Util

let round_float x =
	snd (modf (x +. copysign 0.5 x))

let is_between a b x =
	(min a b) <= x && x <= (max a b)

type vec = {
	x : float;
	y : float;
}

let vec0 = {x = 0.; y = 0.}
let vec1 = {x = 1.; y = 1.}

let ints_of_vec v =
	(int_of_float v.x, int_of_float v.y)

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

let squared_length v =
	v.x**2. +. v.y**2.

let length v =
	sqrt (squared_length v)

let normalize v =
	{x = v.x /. (length v); y  = v.y /. (length v)}

let angle_of_vec v =
	if v = vec0 then 0. else
	let a = acos (dot {x = 1.; y = 0.} (normalize v)) in
	if v.y < 0. then 2. *. pi -. a else a

let reflect vi normal =
	vi -: (2. *. (dot vi normal) *: normal)

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

let ends_of_box center size =
	let a = center -: 0.5 *: size in
	let b = a +: size in
	(a, b)
