open Mlgrope

exception OutOfBoundsException
exception TouchedGoalException
exception CollisionException of entity



val move : Mlgrope.game_state -> float -> Mlgrope.game_state