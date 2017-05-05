open Graphics

open Mlgrope

val run : ('a -> 'a) -> ('a -> Graphics.status -> 'a) -> vec -> 'a -> unit

val draw : Mlgrope.game_state -> unit
