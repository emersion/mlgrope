type vec = {
	x : float;
	y : float;
}

val squared_dist : vec -> vec -> float
val dist : vec -> vec -> float
val (=:) : vec -> vec -> bool
val (+:) : vec -> vec -> vec
val (-:) : vec -> vec -> vec
val ( *:) : float -> vec -> vec
val dot : vec -> vec -> float
val norm : vec -> float
val normalize : vec -> vec
