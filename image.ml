open String
open Graphics

open Util

type image_source =
	| Ppm of in_channel
	| Ppm_file of string
	| Rotation of float * image_source
	| Mirror of image_source

let init_matrix dimx dimy f =
	Array.init dimx (fun i ->
		Array.init dimy (fun j -> f i j)
	)

let matrix_dim m =
	(Array.length m, Array.length m.(0))

(* This color will be replaced by a transparent background *)
let alpha_color = 0xFF00FF

let rec input_ppm_line ch =
	let l = input_line ch in
	if String.length l > 0 && l.[0] <> '#' then l else input_ppm_line ch

let input_ppm ch =
	set_binary_mode_in ch true;
	let is_bw = (input_ppm_line ch <> "P6") in
	let (w, h )= Scanf.sscanf (input_ppm_line ch) "%d %d" (fun x y -> (x, y)) in
	let _ = input_ppm_line ch in
	let m = Array.make_matrix h w (rgb 0 0 0) in
	for i = 0 to h - 1 do
		for j = 0 to w - 1 do
			let (r, g, b) =
				if is_bw then
					let x = input_byte ch in
					(x, x, x)
				else
					let r = input_byte ch in
					let g = input_byte ch in
					let b = input_byte ch in
					(r, g, b)
			in
			let c = Graphics.rgb r g b in
			let c = if c = alpha_color then Graphics.transp else c in
			m.(i).(j) <- c
		done
	done;
	close_in ch;
	m

let mirror m =
	let (h, w) = matrix_dim m in
	init_matrix h w (fun i j -> m.(i).(w-j-1))

let rotate angle m =
	(* TODO: support for images with w <> h *)
	let pos = int_of_float (round_float (angle /. (0.5 *. pi))) in
	if pos = 0 then m else
	let (h, w) = matrix_dim m in
	let f = match pos with
	| 1 -> (fun i j -> m.(j).(i))
	| 2 -> (fun i j -> m.(h-i-1).(w-j-1))
	| 3 -> (fun i j -> m.(w-j-1).(h-i-1))
	| _ -> raise (Failure "Invalid angle")
	in
	init_matrix h w f

let load src =
	let rec load src =
		match src with
		| Ppm(ch) -> input_ppm ch
		| Ppm_file(path) -> input_ppm (open_in path)
		| Rotation(angle, src) -> rotate angle (load src)
		| Mirror(src) -> mirror (load src)
	in
	Graphics.make_image (load src)

(* Lazily loads an image *)
let get src =
	let cache = ref None in
	fun () ->
		match !cache with
		| Some(img) -> img
		| None ->
			let img = load src in
			cache := Some(img);
			img
