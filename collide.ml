open Math2d

let box_point a b pos =
	is_between a.x b.x pos.x && is_between a.y b.y pos.y

let line_equation a b =
	let k = (b.y -. a.y) /. (b.x -. a.x) in
	let y0 = a.y -. a.x *. k in
	let f x = k *. x +. y0 in
	(k, y0, f)

let line_point a b pos =
	let (k, y0, f) = line_equation a b in
	abs_float ((f pos.x) -. pos.y) <= 5.

let lines a b a' b' =
	(*
	k*x + y0 = k'*x + y0'
	x*(k - k') = y0' - y0
	x = (y0' - y0) / (k - k')
	*)
	if a == a' && b == b' then Some(a) else
	let (k, y0, f) = line_equation a b in
	let (k', y0', f') = line_equation a' b' in
	let x = (y0' -. y0) /. (k -. k') in
	let (y, y') = (f x, f' x) in
	if abs_float (y -. y') <= 1. then Some({x; y}) else None

let segment_point a b pos =
	box_point a b pos && line_point a b pos

let segments a b a' b' =
	match lines a b a' b' with
	| Some(pt) -> box_point a b pt && box_point a' b' pt
	| None -> false
