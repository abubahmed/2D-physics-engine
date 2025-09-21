open! Core
open! Graphics
open! Objects

let draw_ball (ball : Ball.t) =
  let x = Units.to_pixels ball.center.x in
  let y = Units.to_pixels ball.center.y in
  let radius = Units.to_pixels ball.radius in
  Graphics.set_color Graphics.blue;
  Graphics.fill_circle x y radius
;;

let draw_line (line : Line.t) : unit =
  let x1 = Units.to_pixels line.first_endp.x in
  let y1 = Units.to_pixels line.first_endp.y in
  let x2 = Units.to_pixels line.second_endp.x in
  let y2 = Units.to_pixels line.second_endp.y in
  Graphics.set_line_width 2;
  Graphics.set_color Graphics.green;
  Graphics.moveto x1 y1;
  Graphics.lineto x2 y2
;;

let draw_cup (cup : Cup.t) : unit =
  let x1 = Units.to_pixels cup.min.x in
  let y1 = Units.to_pixels cup.min.y in
  let x2 = Units.to_pixels cup.max.x in
  let y2 = Units.to_pixels cup.max.y in
  Graphics.set_line_width 2;
  Graphics.set_color Graphics.red;
  Graphics.moveto x1 y2;
  Graphics.lineto x1 y1;
  Graphics.lineto x2 y1;
  Graphics.lineto x2 y2
;;

let draw_box (box : Box.t) : unit =
  let x1 = Units.to_pixels box.min.x in
  let y1 = Units.to_pixels box.min.y in
  let x2 = Units.to_pixels box.max.x in
  let y2 = Units.to_pixels box.max.y in
  Graphics.set_color Graphics.yellow;
  Graphics.fill_rect x1 y1 (x2 - x1) (y2 - y1)
;;

let draw_objects (ui : Interface.t) =
  List.iter ui.canvas.balls ~f:(fun ball -> draw_ball ball);
  List.iter ui.canvas.lines ~f:(fun line -> draw_line line);
  List.iter ui.canvas.cups ~f:(fun cup -> draw_cup cup);
  List.iter ui.canvas.boxes ~f:(fun box -> draw_box box)
;;

let draw_text_box ~display_text ~x ~y ~width ~height =
  let text_width, text_height = Graphics.text_size display_text in
  Graphics.moveto x (y + (height / 2) - (text_height / 2));
  Graphics.set_color Graphics.black;
  Graphics.draw_string display_text
;;

let draw_text_boxes (ui : Interface.t) =
  List.iter ui.panel.text_boxes ~f:(fun text_box ->
    draw_text_box
      ~display_text:text_box.display_text
      ~x:text_box.position.x
      ~y:text_box.position.y
      ~width:text_box.width
      ~height:text_box.height)
;;

let draw_button ~display_text ~x ~y ~width ~height ~color =
  Graphics.set_color color;
  Graphics.fill_rect x y width height;
  let text_width, text_height = Graphics.text_size display_text in
  Graphics.moveto
    (x + (width / 2) - (text_width / 2))
    (y + (height / 2) - (text_height / 2));
  Graphics.set_color Graphics.black;
  Graphics.draw_string display_text
;;

let draw_buttons (ui : Interface.t) =
  List.iter ui.panel.buttons ~f:(fun button ->
    draw_button
      ~display_text:button.display_text
      ~x:button.position.x
      ~y:button.position.y
      ~width:button.width
      ~height:button.height
      ~color:button.color)
;;

let draw_canvas (ui : Interface.t) =
  Graphics.set_color Graphics.black;
  Graphics.fill_rect 0 0 ui.width ui.height;
  draw_objects ui
;;

let draw_panel (ui : Interface.t) =
  Graphics.set_color (Graphics.rgb 128 128 128);
  Graphics.fill_rect ui.canvas.width 0 ui.panel.width ui.panel.height;
  draw_buttons ui;
  draw_text_boxes ui
;;

let draw_ui (ui : Interface.t) =
  Graphics.open_graph " 20000 x 20000 ";
  Graphics.resize_window ui.width ui.height;
  draw_canvas ui;
  draw_panel ui
;;

let init_exn (ui : Interface.t) = draw_ui ui

let render (ui : Interface.t) =
  draw_canvas ui;
  draw_panel ui
;;
