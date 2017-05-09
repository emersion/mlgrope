open Math2d

let box_point a b pos =
	is_between a.x b.x pos.x && is_between a.y b.y pos.y

let line_equation a b =
	let k = (b.y -. a.y) /. (b.x -. a.x) in
	let y0 = a.y -. a.x *. k in
	let f x = k *. x +. y0 in
	(k, y0, f)

let line_point a b pos =
	if a.x = b.x then a.x = pos.x else
	let (k, y0, f) = line_equation a b in
	abs_float ((f pos.x) -. pos.y) <= 5.

let line_vertical a b x =
	if a.x = b.x then
		if a.x = x then Some(a) else None
	else
		let (_, _, f) = line_equation a b in
		Some({x; y = f x})

let lines a b a' b' =
	(*
	k*x + y0 = k'*x + y0'
	x*(k - k') = y0' - y0
	x = (y0' - y0) / (k - k')
	*)
	if a =: a' && b =: b' then Some(a) else
	if a.x = b.x then line_vertical a' b' a.x else
	if a'.x = b'.x then line_vertical a b a'.x else
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

(* Ray casting algorithm *)
let polygon_point l pos =
	let (a, b) = (pos, {pos with x = infinity}) in
	let n = fold_segments (fun n (a', b') ->
		if segments a b a' b' then n+1 else n
	) 0 l in
	n mod 2 = 1

let circle_point center radius pos =
	radius *. radius >= squared_distance center pos

let circle_line center radius p1 p2 =
	let u = p2 -: p1 in
	let ac = center -: p1 in
	let d = (Math2d.length ({x = u.x *. ac.y -. u.y *. ac.x; y = 0.})) /. (Math2d.length u) in
	radius <= d

let cercle_seg center radius p1 p2 =
	circle_line center radius p1 p2
	&& ( Math2d.dot (p2 -: p1) (center -: p1) >= 0. && Math2d.dot (p1 -: p2) (center -: p2) >= 0.
			|| circle_point center radius p1
			|| circle_point center radius p2
			)
