open Graphics

open Mlgrope

val vec_of_status : status -> vec
val draw : Mlgrope.game_state -> unit
val step : (unit -> unit) -> unit
val run : ('a -> 'a) -> ('a -> status -> status -> 'a) -> vec -> 'a -> unit
