open Graphics

let alpha_color = 0xFF00FF

let input e =
	set_binary_mode_in e true;
	let is_bw = (input_line e <> "P6") in
	let w, h =
		let l = ref (input_line e) in
		while !l.[0] = '#' do l := input_line e done;
		Scanf.sscanf !l "%d %d" (fun x y -> x, y)
	in
	let _ = input_line e in
	let m = Array.make_matrix h w (rgb 0 0 0) in
	for i = 0 to h - 1 do
		for j = 0 to w - 1 do
			let r, g, b =
				if is_bw then
					let x = input_byte e in
					x, x, x
				else
					let r = input_byte e in
					let g = input_byte e in
					let b = input_byte e in
					r, g, b
			in
			let c = rgb r g b in
			let c = if c = alpha_color then Graphics.transp else c in
			m.(i).(j) <- c
		done
	done;
	close_in e;
	Graphics.make_image m
