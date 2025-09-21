open! Core
open! Async
open! Graphics
open! Objects

let handle_state_on_button_click (t : World.t) btn_click_state id =
  if Click_state.equal t.click_state btn_click_state
  then t.click_state <- Click_state.Free_state
  else t.click_state <- btn_click_state
;;

let on_button_click (t : World.t) id =
  match id with
  | "create-ball-btn" ->
    let create_ball_btn_state =
      Click_state.Create_object_select_first ObjectTypeSelector.Ball
    in
    handle_state_on_button_click t create_ball_btn_state id
  | "create-line-btn" ->
    let create_line_btn_state =
      Click_state.Create_object_select_first ObjectTypeSelector.Line
    in
    handle_state_on_button_click t create_line_btn_state id
  | "create-cup-btn" ->
    let create_cup_btn_state =
      Click_state.Create_object_select_first ObjectTypeSelector.Cup
    in
    handle_state_on_button_click t create_cup_btn_state id
  | "create-box-btn" ->
    let create_box_btn_state =
      Click_state.Create_object_select_first ObjectTypeSelector.Box
    in
    handle_state_on_button_click t create_box_btn_state id
  | "clear-btn" ->
    Canvas.clear t.ui.canvas;
    t.click_state <- Click_state.Free_state
  | "play-pause-btn" ->
    if World_state.equal t.world_state World_state.In_progress
    then (
      t.world_state <- World_state.Paused;
      List.iter t.ui.panel.buttons ~f:(fun button ->
        if String.equal button.id "play-pause-btn"
        then button.display_text <- "Play"))
    else (
      t.world_state <- World_state.In_progress;
      List.iter t.ui.panel.buttons ~f:(fun button ->
        if String.equal button.id "play-pause-btn"
        then button.display_text <- "Pause"))
  | _ -> ()
;;

let handle_select_object_first (t : World.t) (obj : ObjectTypeSelector.t) x y
  =
  if Canvas.in_bounds t.ui.canvas x y
  then (
    let first_selected_pos =
      { Vector.x = Units.to_units x; y = Units.to_units y }
    in
    t.click_state
    <- Click_state.Create_object_select_final (obj, first_selected_pos))
  else
    List.iter t.ui.panel.buttons ~f:(fun button ->
      if Panel.Button.in_bounds button x y then on_button_click t button.id)
;;

let handle_select_object_final (t : World.t) obj first_selected_pos x y =
  if Canvas.in_bounds t.ui.canvas x y
  then (
    let second_selected_pos =
      { Vector.x = Units.to_units x; y = Units.to_units y }
    in
    let dist = Vector.dist first_selected_pos second_selected_pos in
    let min_pos, max_pos =
      find_min_max first_selected_pos second_selected_pos
    in
    (match obj with
     | ObjectTypeSelector.Ball ->
       let new_ball = Ball.create ~center:first_selected_pos ~radius:dist in
       Canvas.add_ball t.ui.canvas new_ball
     | ObjectTypeSelector.Cup ->
       let new_cup = Objects.Cup.create ~min:min_pos ~max:max_pos in
       Canvas.add_cup t.ui.canvas new_cup
     | ObjectTypeSelector.Box ->
       let new_box = Objects.Box.create ~min:min_pos ~max:max_pos in
       Canvas.add_box t.ui.canvas new_box
     | ObjectTypeSelector.Line ->
       let new_line =
         Line.create
           ~first_endp:first_selected_pos
           ~second_endp:second_selected_pos
       in
       Canvas.add_line t.ui.canvas new_line);
    t.click_state <- Click_state.Free_state)
  else
    List.iter t.ui.panel.buttons ~f:(fun button ->
      if Panel.Button.in_bounds button x y then on_button_click t button.id)
;;

let handle_free_state (t : World.t) x y =
  let buttons = t.ui.panel.buttons in
  List.iter buttons ~f:(fun button ->
    if Panel.Button.in_bounds button x y then on_button_click t button.id);
  let point = { Vector.x = Units.to_units x; y = Units.to_units y } in
  let iter_obj_select f wrap lst =
    List.iter lst ~f:(fun obj ->
      if f obj point
      then t.click_state <- Click_state.Drag_current_object (wrap obj))
  in
  iter_obj_select
    Collisions.ball_point_collide
    ObjectSelector.(fun b -> Ball b)
    t.ui.canvas.balls;
  iter_obj_select
    Collisions.box_point_collide
    ObjectSelector.(fun b -> Box b)
    t.ui.canvas.boxes;
  iter_obj_select
    Collisions.line_point_collide
    ObjectSelector.(fun l -> Line l)
    t.ui.canvas.lines;
  iter_obj_select
    Collisions.cup_point_collide
    ObjectSelector.(fun c -> Cup c)
    t.ui.canvas.cups
;;

let handle_drag_object (t : World.t) obj x y =
  (match obj with
   | ObjectSelector.Ball ball -> ball.velocity <- Vector.zero ()
   | _ -> ());
  t.click_state <- Click_state.Free_state
;;

let handle_select_object t obj x y =
  ignore t;
  ignore obj
;;

let handle_free_state (t : World.t) x y =
  let buttons = t.ui.panel.buttons in
  List.iter buttons ~f:(fun button ->
    if Panel.Button.in_bounds button x y then on_button_click t button.id);
  let point = { Vector.x = Units.to_units x; y = Units.to_units y } in
  let iter_obj_select f wrap lst =
    List.iter lst ~f:(fun obj ->
      if f obj point
      then t.click_state <- Click_state.Drag_current_object (wrap obj))
  in
  iter_obj_select
    Collisions.ball_point_collide
    ObjectSelector.(fun b -> Ball b)
    t.ui.canvas.balls;
  iter_obj_select
    Collisions.box_point_collide
    ObjectSelector.(fun b -> Box b)
    t.ui.canvas.boxes;
  iter_obj_select
    Collisions.line_point_collide
    ObjectSelector.(fun l -> Line l)
    t.ui.canvas.lines;
  iter_obj_select
    Collisions.cup_point_collide
    ObjectSelector.(fun c -> Cup c)
    t.ui.canvas.cups
;;

let handle_button_down_click (t : World.t) x y =
  match t.click_state with
  | Click_state.Create_object_select_first obj ->
    handle_select_object_first t obj x y
  | Click_state.Create_object_select_final (obj, first_selected_pos) ->
    handle_select_object_final t obj first_selected_pos x y
  | Click_state.Drag_current_object obj -> handle_drag_object t obj x y
  | Click_state.Select_current_object obj -> handle_select_object t obj x y
  | Click_state.Free_state -> handle_free_state t x y
;;

let handle_button_up_click (t : World.t) x y =
  match t.click_state with
  | Click_state.Drag_current_object obj -> handle_drag_object t obj x y
  | _ -> ()
;;

let rec handle_click (t : World.t) : unit Deferred.t =
  let%bind event =
    In_thread.run (fun () ->
      let event = wait_next_event [ Button_down; Button_up ] in
      event)
  in
  let x = event.mouse_x in
  let y = event.mouse_y in
  if event.button
  then (
    handle_button_down_click t x y;
    handle_click t)
  else (
    handle_button_up_click t x y;
    handle_click t)
;;
