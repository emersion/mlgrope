open Graphics

open Mlgrope

val run : ('a -> 'a) -> ('a -> Graphics.status -> Graphics.status -> 'a) -> vec -> 'a -> unit

val vec_of_status : Graphics.status -> vec
val draw : Mlgrope.game_state -> unit
