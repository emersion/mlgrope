open Math2d

val box_point : vec -> vec -> vec -> bool
val line_point : vec -> vec -> vec -> bool
val lines : vec -> vec -> vec -> vec -> vec option
val segment_point : vec -> vec -> vec -> bool
val segments : vec -> vec -> vec -> vec -> bool
val polygon_point : vec list -> vec -> bool
val circle_point : vec -> float -> vec -> bool
