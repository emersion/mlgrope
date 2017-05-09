open Util
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
	| Some(pt) ->
		if box_point a b pt && box_point a' b' pt then Some(pt) else None
	| None -> None

let circle_point center radius pos =
	radius *. radius >= squared_distance center pos

let circle_line center radius a b =
	let u = b -: a in
	let ac = center -: a in
	let d = (Math2d.length {x = u.x *. ac.y -. u.y *. ac.x; y = 0.}) /. (Math2d.length u) in
	if radius <= d then Some(a) else None

let circle_segment center radius a b =
	match circle_line center radius a b with
	| Some(pt) ->
		if Math2d.dot (b -: a) (center -: a) >= 0.
			&& Math2d.dot (a -: b) (center -: b) >= 0.
			|| circle_point center radius a
			|| circle_point center radius b
		then Some(pt) else None
	| None -> None

let circle_seg_inter a b center =
	let u = b -: a in
	let ac = center -: a in
	let i = a +: ((dot u ac) /. squared_length u) *: u in
	i

let circle_seg_norm a b center =
	let u = b -: a in
	let ac = center -: a in
	let n = {x = -.u.y *. (u.x*.ac.y -. u.y*.ac.x); y = u.x *. (u.x*.ac.y -. u.y*.ac.x)} in
	normalize n

(* Ray casting algorithm *)
let polygon_point l pos =
	let (a, b) = (pos, {pos with x = infinity}) in
	let n = fold_segments (fun n (a', b') ->
		if is_some (segments a b a' b') then n+1 else n
	) 0 l in
	n mod 2 = 1

let polygon_line l a b =
	None (* TODO *)

let polygon_segment l a b =
	None (* TODO *)

let polygon_circle l center radius =
	fold_segments (fun acc (a, b) ->
		if is_some (circle_segment center radius a b)
		then (a, b)::acc
		else acc
	) [] l
