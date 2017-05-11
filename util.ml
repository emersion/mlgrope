let pi = 3.14159265358979323846264338327950288419716939937510

let is_some opt =
	match opt with Some(_) -> true | None -> false

let round_float x =
	snd (modf (x +. copysign 0.5 x))
