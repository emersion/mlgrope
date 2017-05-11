type vec = {
	x : float;
	y : float;
}

val vec0 : vec
val vec1 : vec

val ints_of_vec : vec -> (int * int)
val map : (float -> float) -> vec -> vec
val squared_distance : vec -> vec -> float
val distance : vec -> vec -> float
val (=:) : vec -> vec -> bool
val (+:) : vec -> vec -> vec
val (-:) : vec -> vec -> vec
val ( *:) : float -> vec -> vec
val dot : vec -> vec -> float
val cross : vec -> vec
val squared_length : vec -> float
val length : vec -> float
val normalize : vec -> vec
val vec_of_angle : float -> vec
val angle_of_vec : vec -> float
val reflect : vec -> vec -> vec
val rotate : float -> vec -> vec
val fold_segments : ('a -> (vec * vec) -> 'a) -> 'a -> vec list -> 'a
val average : vec list -> vec
val ends_of_box : vec -> vec -> (vec * vec)
