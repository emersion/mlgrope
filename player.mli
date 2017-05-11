open Math2d
open Mlgrope

exception TouchedGoalException of game_state * vec
exception OutOfBoundsException of game_state

val run : vec -> game_state -> unit
