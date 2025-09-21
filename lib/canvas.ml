open! Core
open! Objects

type t =
  { height : int
  ; width : int
  ; mutable balls : Ball.t list
  ; mutable lines : Line.t list
  ; mutable cups : Cup.t list
  ; mutable boxes : Box.t list
  }

let create ~height ~width =
  { height; width; balls = []; lines = []; cups = []; boxes = [] }
;;

let in_bounds t x y = x >= 0 && x < t.width && y >= 0 && y < t.height

let bound_objects t =
  let box_width = Units.to_units t.width in
  let box_height = Units.to_units t.height in
  List.iter t.balls ~f:(fun ball ->
    let radius = ball.radius in
    let min_x = radius in
    let max_x = box_width -. radius in
    let clamped_x =
      if Float.( < ) ball.center.x min_x
      then min_x
      else if Float.( > ) ball.center.x max_x
      then max_x
      else ball.center.x
    in
    let min_y = radius in
    let max_y = box_height -. radius in
    let clamped_y =
      if Float.( < ) ball.center.y min_y
      then min_y
      else if Float.( > ) ball.center.y max_y
      then max_y
      else ball.center.y
    in
    let hit_left_wall = Float.equal clamped_x min_x in
    let hit_right_wall = Float.equal clamped_x max_x in
    let hit_top_wall = Float.equal clamped_y min_y in
    let hit_bottom_wall = Float.equal clamped_y max_y in
    let new_velocity_x =
      if hit_left_wall || hit_right_wall
      then -.ball.velocity.x *. Constants.coeff_of_restitution
      else ball.velocity.x
    in
    let new_velocity_y =
      if hit_top_wall || hit_bottom_wall
      then -.ball.velocity.y *. Constants.coeff_of_restitution
      else ball.velocity.y
    in
    let new_velocity_vector =
      { Vector.x = new_velocity_x; y = new_velocity_y }
    in
    let new_velocity_vector =
      if
        Float.( < )
          (Vector.mag new_velocity_vector)
          Constants.velocity_threshold
      then Vector.zero ()
      else new_velocity_vector
    in
    ball.velocity <- new_velocity_vector;
    ball.center <- { x = clamped_x; y = clamped_y })
;;

let add_ball t obj = t.balls <- t.balls @ [ obj ]
let add_line t obj = t.lines <- t.lines @ [ obj ]
let add_cup t obj = t.cups <- t.cups @ [ obj ]
let add_box t obj = t.boxes <- t.boxes @ [ obj ]

let clear t =
  t.balls <- [];
  t.cups <- [];
  t.lines <- [];
  t.boxes <- []
;;
