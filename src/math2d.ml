open Util

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

let cross v =
	{x = -. v.y; y = v.x}

let squared_length v =
	v.x**2. +. v.y**2.

let length v =
	sqrt (squared_length v)

let normalize v =
	{x = v.x /. (length v); y  = v.y /. (length v)}

let vec_of_angle a =
	{x = cos a; y = sin a}

let angle_of_vec v =
	if v = vec0 then 0. else
	let a = acos (dot {x = 1.; y = 0.} (normalize v)) in
	if v.y < 0. then 2. *. pi -. a else a

let reflect vi normal =
	vi -: (2. *. (dot vi normal) *: normal)

let rotate a v =
	{
		x = v.x *. (cos a) +. v.y *. (sin a);
		y = -. v.x *. (sin a) +. v.y *. (cos a);
	}

let fold_segments f acc l =
	match l with
	| h::t ->
		let (last, acc) = List.fold_left (fun (last, acc) v ->
			(v, f acc (last, v))
		) (h, acc) t in
		f acc (last, h) (* Close last segment *)
	| [] -> acc

let average l =
	let (sum, n) = List.fold_left (fun (sum, n) v -> (sum +: v, n+1)) (vec0, 0) l in
	(1. /. float_of_int n) *: sum

let ends_of_box center size =
	let a = center -: 0.5 *: size in
	let b = a +: size in
	(a, b)

let mix_vec v1 v2 t =
	{ x = mix v1.x v2.x t; y = mix v1.y v2.y t }
