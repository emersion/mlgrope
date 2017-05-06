open Math2d
open Mlgrope

exception TouchedGoalException

val move : game_state -> float -> game_state

val check_collision : vec -> entity -> bool
