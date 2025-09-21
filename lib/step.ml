open! Core
open! Graphics
open! Objects
open! Core
open! Graphics
open! Objects

let step_click_state_display_text (t : World.t) dt =
  ignore dt;
  let panel = t.ui.panel in
  let click_state_text_option =
    Panel.find_textbox panel "click-state-text"
  in
  match click_state_text_option with
  | Some click_state_text ->
    click_state_text.display_text <- Click_state.to_string t.click_state
  | None -> failwith "textbox click-state-text not found"
;;

let drag_box_cup_object current_mouse_pos (min : Vector.t) (max : Vector.t) =
  let width = max.x -. min.x in
  let height = max.y -. min.y in
  let new_min_pos =
    Vector.translate_xy current_mouse_pos (width /. -2.0) (height /. -2.0)
  in
  let new_max_pos =
    Vector.translate_xy current_mouse_pos (width /. 2.0) (height /. 2.0)
  in
  new_min_pos, new_max_pos
;;

let update_stats_textbox (t : World.t) id display_text =
  let panel = t.ui.panel in
  match Panel.find_textbox panel id with
  | Some textbox -> textbox.display_text <- display_text
  | None -> failwith ("textbox " ^ id ^ " not found")
;;

let step_show_object_stats (t : World.t) dt =
  ignore dt;
  List.iter
    [ "object-stats-velocity-text", "Velocity:"
    ; "object-stats-center-text", "Center:"
    ; "object-stats-mass-text", "Mass:"
    ; "object-stats-speed-text", "Speed:"
    ; "object-stats-momentum-text", "Momentum:"
    ; "object-stats-ke-text", "Kinetic E:"
    ; "object-stats-pe-text", "Potential E:"
    ; "object-stats-me-text", "Mechanical E:"
    ]
    ~f:(fun (id, text) -> update_stats_textbox t id text);
  let mouse_x, mouse_y = mouse_pos () in
  let current_mouse_pos =
    { Vector.x = Units.to_units mouse_x; y = Units.to_units mouse_y }
  in
  List.iter t.ui.canvas.balls ~f:(fun ball ->
    if Collisions.ball_point_collide ball current_mouse_pos
    then (
      let ball_vel = ball.velocity in
      let ball_center = ball.center in
      let ball_mass = ball.mass in
      let ball_vel_mag = Vector.mag ball_vel in
      let ball_momentum = Vector.( * ) ball_vel ball_mass in
      let ball_ke = ball_mass *. ball_vel_mag *. ball_vel_mag /. 2.0 in
      let ball_pe =
        -.ball_mass *. Constants.g *. (ball_center.y -. ball.radius)
      in
      let ball_me = ball_ke +. ball_pe in
      let ball_vel_text =
        Printf.sprintf "Velocity: (%.2f, %.2f)" ball_vel.x ball_vel.y
      in
      let ball_center_text =
        Printf.sprintf "Center: (%.2f, %.2f)" ball_center.x ball_center.y
      in
      let ball_mass_text = Printf.sprintf "Mass: %.2f" ball_mass in
      let ball_speed_text = Printf.sprintf "Speed: %.2f" ball_vel_mag in
      let ball_momentum_text =
        Printf.sprintf
          "Momentum: (%.2f, %.2f)"
          ball_momentum.x
          ball_momentum.y
      in
      let ball_ke_text = Printf.sprintf "Kinetic E: %.2f" ball_ke in
      let ball_pe_text = Printf.sprintf "Potential E: %.2f" ball_pe in
      let ball_me_text = Printf.sprintf "Mechanical E: %.2f" ball_me in
      List.iter
        [ "object-stats-velocity-text", ball_vel_text
        ; "object-stats-center-text", ball_center_text
        ; "object-stats-mass-text", ball_mass_text
        ; "object-stats-speed-text", ball_speed_text
        ; "object-stats-momentum-text", ball_momentum_text
        ; "object-stats-ke-text", ball_ke_text
        ; "object-stats-pe-text", ball_pe_text
        ; "object-stats-me-text", ball_me_text
        ]
        ~f:(fun (id, text) -> update_stats_textbox t id text)))
;;

let step_drag_object (t : World.t) dt =
  ignore dt;
  let mouse_x, mouse_y = mouse_pos () in
  let current_mouse_pos =
    { Vector.x = Units.to_units mouse_x; y = Units.to_units mouse_y }
  in
  match t.click_state with
  | Click_state.Drag_current_object obj ->
    (match obj with
     | Ball ball -> ball.center <- current_mouse_pos
     | Box box ->
       let new_min_pos, new_max_pos =
         drag_box_cup_object current_mouse_pos box.min box.max
       in
       box.min <- new_min_pos;
       box.max <- new_max_pos
     | Line line ->
       let line_center = Objects.Line.center line in
       let vector_to_first_endp = Vector.( - ) line.first_endp line_center in
       let vector_to_second_endp =
         Vector.( - ) line.second_endp line_center
       in
       line.first_endp <- Vector.( + ) current_mouse_pos vector_to_first_endp;
       line.second_endp
       <- Vector.( + ) current_mouse_pos vector_to_second_endp
     | Cup cup ->
       let new_min_pos, new_max_pos =
         drag_box_cup_object current_mouse_pos cup.min cup.max
       in
       cup.min <- new_min_pos;
       cup.max <- new_max_pos)
  | _ -> ()
;;

let step_positions (t : World.t) dt =
  List.iter t.ui.canvas.balls ~f:(fun ball -> Ball.update_pos ball dt);
  Canvas.bound_objects t.ui.canvas
;;

let step_velocities (t : World.t) dt =
  List.iter t.ui.canvas.balls ~f:(fun ball -> Ball.update_vel ball dt)
;;

let step_reactions (t : World.t) dt =
  ignore dt;
  Reactions.react t.ui.canvas
;;

let step (t : World.t) dt steps =
  if
    World_state.equal t.world_state World_state.In_progress
    && Click_state.equal t.click_state Click_state.Free_state
  then (
    step_reactions t dt;
    step_velocities t dt;
    step_positions t dt);
  step_drag_object t dt;
  step_click_state_display_text t dt;
  if steps mod Constants.every_step_stats = 0
  then step_show_object_stats t dt
;;
