open Math2d
open Mlgrope

exception TouchedGoalException of vec

val move : game_state -> float -> game_state
