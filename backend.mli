open Mlgrope

exception OutOfBoundsException
exception TouchedGoalException

val move : Mlgrope.game_state -> float -> Mlgrope.game_state
