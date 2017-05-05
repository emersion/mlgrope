open Mlgrope

exception TouchedGoalException

val move : Mlgrope.game_state -> float -> Mlgrope.game_state

val check_collision : Mlgrope.vec -> Mlgrope.entity -> bool
