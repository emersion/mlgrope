let pi = 3.14159265358979323846264338327950288419716939937510

let is_some opt =
	match opt with Some(_) -> true | None -> false

let round_float x =
	snd (modf (x +. copysign 0.5 x))

let is_between a b x =
	(min a b) <= x && x <= (max a b)

let normalize_angle a =
	mod_float a (2. *. pi)
