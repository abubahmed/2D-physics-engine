open! Core
open! Async
open! Objects
open! Vector

let ball_point_collide (ball : Ball.t) (point : Vector.t) =
  let ball_point_dist_squared = Vector.dist_squared ball.center point in
  let radius_squared = ball.radius *. ball.radius in
  Float.( <= ) ball_point_dist_squared radius_squared
;;

let box_point_collide (box : Box.t) (point : Vector.t) =
  Float.( <= ) point.x box.max.x
  && Float.( >= ) point.x box.min.x
  && Float.( <= ) point.y box.max.y
  && Float.( >= ) point.y box.min.y
;;

let cup_point_collide (cup : Cup.t) (point : Vector.t) =
  Float.( <= ) point.x cup.max.x
  && Float.( >= ) point.x cup.min.x
  && Float.( <= ) point.y cup.max.y
  && Float.( >= ) point.y cup.min.y
;;

let line_point_collide (line : Line.t) (point : Vector.t) =
  let dist_first_endp = Vector.dist line.first_endp point in
  let dist_second_endp = Vector.dist line.second_endp point in
  let line_length = Line.length line in
  let dist_diff =
    Float.abs (dist_first_endp +. dist_second_endp -. line_length)
  in
  Float.( <= ) dist_diff Constants.line_select_tolerance
;;

let ball_and_ball (ball1 : Ball.t) (ball2 : Ball.t) : bool =
  Float.( < )
    (Vector.mag (Vector.( - ) ball1.center ball2.center))
    (ball1.radius +. ball2.radius)
;;

let ball_and_ball_collision_point (ball1 : Ball.t) (ball2 : Ball.t)
  : Vector.t
  =
  Vector.( / ) (Vector.( + ) ball1.center ball2.center) 2.0
;;

let ball_and_line (ball : Ball.t) (line : Line.t) : bool =
  let vector_A = Vector.( - ) ball.center line.first_endp in
  let vector_B = Vector.( - ) line.second_endp line.first_endp in
  let dot = Vector.dot_product vector_A vector_B in
  let len_sq = Vector.dot_product vector_B vector_B in
  let t =
    if Float.equal len_sq 0.0
    then 0.0
    else (
      let proj = dot /. len_sq in
      if Float.( < ) proj 0.0
      then 0.0
      else if Float.( > ) proj 1.0
      then 1.0
      else proj)
  in
  let closest_point =
    Vector.( + ) line.first_endp (Vector.scale vector_B ~k:t)
  in
  let dist = Vector.dist ball.center closest_point in
  Float.( <= ) dist ball.radius
;;

let ball_and_line_collision_point (ball : Ball.t) (line : Line.t) : Vector.t =
  let vector_A = Vector.( - ) ball.center line.first_endp in
  let vector_B = Vector.( - ) line.second_endp line.first_endp in
  let projection =
    Vector.( * )
      vector_B
      (Vector.dot_product vector_A vector_B
       /. Vector.dot_product vector_B vector_B)
  in
  Vector.( + ) projection line.first_endp
;;

let ball_in_cup (ball : Ball.t) (cup : Cup.t) : bool =
  print_s [%sexp (ball.center : Vector.t)];
  print_s [%sexp (cup.min : Vector.t)];
  print_s [%sexp (cup.max : Vector.t)];
  Float.compare ball.center.x (cup.min.x +. ball.radius) >= 0
  && Float.compare ball.center.x (cup.max.x -. ball.radius) <= 0
  && Float.compare ball.center.y (cup.min.y +. ball.radius) >= 0
  && Float.compare ball.center.y (cup.max.y -. ball.radius) <= 0
;;

let ball_and_line_collision_point (ball : Ball.t) (line : Line.t) : Vector.t =
  let vector_A = Vector.( - ) ball.center line.first_endp in
  let vector_B = Vector.( - ) line.second_endp line.first_endp in
  let projection =
    Vector.( * )
      vector_B
      (Vector.dot_product vector_A vector_B
       /. Vector.dot_product vector_B vector_B)
  in
  Vector.( + ) projection line.first_endp
;;

let ball_resting_in_cup (ball : Ball.t) (cup : Cup.t) : bool =
  Float.compare ball.center.x (cup.min.x +. ball.radius) > 0
  && Float.compare ball.center.x (cup.max.x -. ball.radius) < 0
  && Float.compare ball.center.y (cup.min.y +. ball.radius) = 0
;;

let ball_collides_with_cup_wall (ball : Ball.t) (cup : Cup.t) : bool =
  (Float.compare ball.center.y (cup.min.y +. ball.radius) > 0
   && Float.compare ball.center.y (cup.max.y -. ball.radius) < 0)
  && (Float.compare ball.center.x (cup.min.x -. ball.radius) = 0
      || Float.compare ball.center.x (cup.min.x +. ball.radius) = 0
      || Float.compare ball.center.x (cup.max.x -. ball.radius) = 0
      || Float.compare ball.center.x (cup.max.x +. ball.radius) = 0)
;;

let ball_collides_with_cup_wall_collision_point (ball : Ball.t) (cup : Cup.t)
  =
  if Float.compare ball.center.x (cup.min.x -. ball.radius) = 0
  then { x = ball.center.x +. ball.radius; y = ball.center.y }
  else if Float.compare ball.center.x (cup.min.x -. ball.radius) = 0
  then { x = ball.center.x -. ball.radius; y = ball.center.y }
  else if Float.compare ball.center.x (cup.min.x -. ball.radius) = 0
  then { x = ball.center.x +. ball.radius; y = ball.center.y }
  else { x = ball.center.x -. ball.radius; y = ball.center.y }
;;

let ball_collides_with_cup_bottom (ball : Ball.t) (cup : Cup.t) : bool =
  (Float.compare ball.center.x (cup.min.x +. ball.radius) >= 0
   && Float.compare ball.center.x (cup.max.x -. ball.radius) <= 0)
  && Float.compare ball.center.y (cup.min.y -. ball.radius) = 0
;;

let ball_collides_with_cup_bottom_collision_point
      (ball : Ball.t)
      (cup : Cup.t)
  : Vector.t
  =
  { x = ball.center.x; y = ball.center.y +. ball.radius }
;;

let ball_collides_with_box (ball : Ball.t) (box : Box.t) =
  let ball_x = ball.center.x in
  let ball_y = ball.center.y in
  let radius = ball.radius in
  let closest_x = Float.min (Float.max ball_x box.min.x) box.max.x in
  let closest_y = Float.min (Float.max ball_y box.min.y) box.max.y in
  let dx = ball_x -. closest_x in
  let dy = ball_y -. closest_y in
  let distance_squared = (dx *. dx) +. (dy *. dy) in
  Float.( <= ) distance_squared (radius *. radius)
;;

let ball_collides_with_box_collision_point (ball : Ball.t) (box : Box.t)
  : Vector.t
  =
  let left_side =
    Float.compare (ball.center.x +. ball.radius) box.min.x = 0
    && Float.compare ball.center.y box.min.y > 0
    && Float.compare ball.center.y box.max.y < 0
  in
  let top_side =
    Float.compare (ball.center.y -. ball.radius) box.max.y = 0
    && Float.compare ball.center.x box.min.x > 0
    && Float.compare ball.center.x box.max.x < 0
  in
  let right_side =
    Float.compare (ball.center.x -. ball.radius) box.min.x = 0
    && Float.compare ball.center.y box.min.y > 0
    && Float.compare ball.center.y box.max.y < 0
  in
  let bottom_side =
    Float.compare (ball.center.y +. ball.radius) box.min.y = 0
    && Float.compare ball.center.x box.min.x > 0
    && Float.compare ball.center.x box.max.x < 0
  in
  let top_left_corner =
    let vec1 = ball.center in
    let vec2 = { x = box.min.x; y = box.max.y } in
    Float.compare (Vector.dist vec1 vec2) ball.radius = 0
  in
  let top_right_corner =
    let vec1 = ball.center in
    let vec2 = { x = box.max.x; y = box.max.y } in
    Float.compare (Vector.dist vec1 vec2) ball.radius = 0
  in
  let bottom_right_corner =
    let vec1 = ball.center in
    let vec2 = { x = box.max.x; y = box.min.y } in
    Float.compare (Vector.dist vec1 vec2) ball.radius = 0
  in
  let bottom_left_corner =
    let vec1 = ball.center in
    let vec2 = { x = box.min.x; y = box.min.y } in
    Float.compare (Vector.dist vec1 vec2) ball.radius = 0
  in
  ignore bottom_left_corner;
  if left_side
  then { x = box.min.x; y = ball.center.y }
  else if top_side
  then { x = ball.center.x; y = box.max.y }
  else if right_side
  then { x = box.max.x; y = ball.center.y }
  else if bottom_side
  then { x = ball.center.x; y = box.min.y }
  else if top_left_corner
  then { x = box.min.x; y = box.max.y }
  else if top_right_corner
  then { x = box.max.x; y = box.max.y }
  else if bottom_right_corner
  then { x = box.max.x; y = box.min.y }
  else { x = box.min.x; y = box.min.y }
;;
