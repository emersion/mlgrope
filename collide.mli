open Math2d

val box_point : vec -> vec -> vec -> bool
val line_point : vec -> vec -> vec -> bool
val lines : vec -> vec -> vec -> vec -> vec option
val segment_point : vec -> vec -> vec -> bool
val segments : vec -> vec -> vec -> vec -> vec option
val polygon_point : vec list -> vec -> bool
val polygon_line : vec list -> vec -> vec -> vec option
val polygon_segment : vec list -> vec -> vec -> vec option
val circle_point : vec -> float -> vec -> bool
val circle_line : vec -> float -> vec -> vec -> vec option
val circle_segment : vec -> float -> vec -> vec -> vec option
