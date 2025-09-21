open! Core
open! Async
open! Objects
open! Vector

val ball_point_collide : Ball.t -> Vector.t -> bool
val box_point_collide : Box.t -> Vector.t -> bool
val cup_point_collide : Cup.t -> Vector.t -> bool
val line_point_collide : Line.t -> Vector.t -> bool

val ball_and_ball : Ball.t -> Ball.t -> bool
val ball_and_ball_collision_point : Ball.t -> Ball.t -> Vector.t

val ball_and_line : Ball.t -> Line.t -> bool
val ball_and_line_collision_point : Ball.t -> Line.t -> Vector.t

val ball_in_cup : Ball.t -> Cup.t -> bool
val ball_resting_in_cup : Ball.t -> Cup.t -> bool

val ball_collides_with_cup_wall : Ball.t -> Cup.t -> bool
val ball_collides_with_cup_wall_collision_point : Ball.t -> Cup.t -> Vector.t

val ball_collides_with_cup_bottom : Ball.t -> Cup.t -> bool
val ball_collides_with_cup_bottom_collision_point : Ball.t -> Cup.t -> Vector.t

val ball_collides_with_box : Ball.t -> Box.t -> bool
val ball_collides_with_box_collision_point : Ball.t -> Box.t -> Vector.t
