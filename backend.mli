open Math2d
open Mlgrope

exception TouchedGoalException
exception DestroyBallException

val move : game_state -> float -> game_state
