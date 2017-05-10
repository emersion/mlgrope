open Graphics

open Math2d
open Mlgrope

val goal_radius : float
val star_radius : float

val mouse_of_status : status -> vec

val draw_entity : entity -> unit
val draw : game_state -> unit
val run : ('a -> 'a) -> ('a -> status -> status -> 'a) -> vec -> 'a -> unit
val close : unit -> unit
