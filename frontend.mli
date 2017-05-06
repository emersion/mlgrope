open Graphics

open Math2d
open Mlgrope

val goal_radius : float
val star_radius : float

val vec_of_status : status -> vec
val draw : game_state -> unit
val step : (unit -> unit) -> unit
val run : ('a -> 'a) -> ('a -> status -> status -> 'a) -> vec -> 'a -> unit
