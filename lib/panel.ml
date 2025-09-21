open! Core
open! Graphics
open! Objects

module TextBox = struct
  type t =
    { height : int
    ; width : int
    ; position : Vector.Plain.t
    ; id : string
    ; mutable display_text : string
    }

  let create ~height ~width ~position ~id ~display_text =
    { height; width; position; id; display_text }
  ;;

  let update_text t ~(display_text : string) = t.display_text <- display_text
end

module Button = struct
  type t =
    { height : int
    ; width : int
    ; position : Vector.Plain.t
    ; id : string
    ; mutable display_text : string
    ; mutable color : Graphics.color
    }

  let create ~height ~width ~position ~id ~display_text ~color =
    { height; width; position; id; display_text; color }
  ;;

  let in_bounds t x y =
    let btn_min_x = t.position.x in
    let btn_max_x = btn_min_x + t.width in
    let btn_min_y = t.position.y in
    let btn_max_y = btn_min_y + t.height in
    x >= btn_min_x && x <= btn_max_x && y >= btn_min_y && y <= btn_max_y
  ;;
end

type t =
  { height : int
  ; width : int
  ; buttons : Button.t list
  ; text_boxes : TextBox.t list
  }

let find_button t (id : string) =
  let buttons = t.buttons in
  List.find buttons ~f:(fun button -> String.equal button.id id)
;;

let find_textbox t (id : string) =
  let text_boxes = t.text_boxes in
  List.find text_boxes ~f:(fun text_box -> String.equal text_box.id id)
;;

let create ~height:ui_height ~width =
  let create_btn_width = width * 3 / 15 in
  let create_btn_height = ui_height / 20 in
  let base_y = 9 * ui_height / 10 in
  let box_y = base_y - 50 in
  let create_button ~x ~y ~id ~text ~color =
    Button.create
      ~height:create_btn_height
      ~width:create_btn_width
      ~position:{ x; y }
      ~id
      ~display_text:text
      ~color
  in
  let create_ball_btn =
    create_button
      ~x:(7 * width * 3 / 10)
      ~y:base_y
      ~id:"create-ball-btn"
      ~text:"Ball"
      ~color:Graphics.white
  in
  let create_line_btn =
    create_button
      ~x:(4 * width * 3 / 5)
      ~y:base_y
      ~id:"create-line-btn"
      ~text:"Line"
      ~color:Graphics.white
  in
  let create_cup_btn =
    create_button
      ~x:(9 * width * 3 / 10)
      ~y:base_y
      ~id:"create-cup-btn"
      ~text:"Cup"
      ~color:Graphics.white
  in
  let create_box_btn =
    create_button
      ~x:(7 * width * 3 / 10)
      ~y:box_y
      ~id:"create-box-btn"
      ~text:"Box"
      ~color:Graphics.white
  in
  let clear_btn =
    Button.create
      ~height:create_btn_height
      ~width:(8 * width / 10)
      ~position:{ x = 7 * width * 3 / 10; y = box_y - 50 }
      ~id:"clear-btn"
      ~display_text:"Clear"
      ~color:Graphics.red
  in
  let play_pause_button =
    Button.create
      ~height:create_btn_height
      ~width:(8 * width / 10)
      ~position:{ x = 7 * width * 3 / 10; y = box_y - 100 }
      ~id:"play-pause-btn"
      ~display_text:"Play"
      ~color:Graphics.white
  in
  let click_state_text =
    TextBox.create
      ~height:create_btn_height
      ~width:(8 * width / 10)
      ~position:{ x = 7 * width * 3 / 10; y = box_y - 150 }
      ~id:"click-state-text"
      ~display_text:""
  in
  let object_stats_velocity =
    TextBox.create
      ~height:create_btn_height
      ~width:(8 * width / 10)
      ~position:{ x = 7 * width * 3 / 10; y = box_y - 180 }
      ~id:"object-stats-velocity-text"
      ~display_text:"Velocity:"
  in
  let object_stats_center =
    TextBox.create
      ~height:create_btn_height
      ~width:(8 * width / 10)
      ~position:
        { x = 7 * width * 3 / 10; y = box_y - 180 - (create_btn_height + 5) }
      ~id:"object-stats-center-text"
      ~display_text:"Center:"
  in
  let object_stats_mass =
    TextBox.create
      ~height:create_btn_height
      ~width:(8 * width / 10)
      ~position:
        { x = 7 * width * 3 / 10
        ; y = box_y - 180 - (2 * (create_btn_height + 5))
        }
      ~id:"object-stats-mass-text"
      ~display_text:"Mass:"
  in
  let object_stats_speed =
    TextBox.create
      ~height:create_btn_height
      ~width:(8 * width / 10)
      ~position:
        { x = 7 * width * 3 / 10
        ; y = box_y - 180 - (3 * (create_btn_height + 5))
        }
      ~id:"object-stats-speed-text"
      ~display_text:"Speed:"
  in
  let object_stats_momentum =
    TextBox.create
      ~height:create_btn_height
      ~width:(8 * width / 10)
      ~position:
        { x = 7 * width * 3 / 10
        ; y = box_y - 180 - (4 * (create_btn_height + 5))
        }
      ~id:"object-stats-momentum-text"
      ~display_text:"Momentum:"
  in
  let object_stats_ke =
    TextBox.create
      ~height:create_btn_height
      ~width:(8 * width / 10)
      ~position:
        { x = 7 * width * 3 / 10
        ; y = box_y - 180 - (5 * (create_btn_height + 5))
        }
      ~id:"object-stats-ke-text"
      ~display_text:"Kinetic Energy:"
  in
  let object_stats_pe =
    TextBox.create
      ~height:create_btn_height
      ~width:(8 * width / 10)
      ~position:
        { x = 7 * width * 3 / 10
        ; y = box_y - 180 - (6 * (create_btn_height + 5))
        }
      ~id:"object-stats-pe-text"
      ~display_text:"Potential Energy:"
  in
  let object_stats_me =
    TextBox.create
      ~height:create_btn_height
      ~width:(8 * width / 10)
      ~position:
        { x = 7 * width * 3 / 10
        ; y = box_y - 180 - (7 * (create_btn_height + 5))
        }
      ~id:"object-stats-me-text"
      ~display_text:"Mechanical Energy:"
  in
  let stats_texts =
    [ object_stats_center
    ; object_stats_ke
    ; object_stats_mass
    ; object_stats_momentum
    ; object_stats_pe
    ; object_stats_speed
    ; object_stats_velocity
    ; object_stats_me
    ]
  in
  { height = ui_height
  ; width
  ; buttons =
      [ create_ball_btn
      ; create_box_btn
      ; create_cup_btn
      ; create_line_btn
      ; clear_btn
      ; play_pause_button
      ]
  ; text_boxes = [ click_state_text ] @ stats_texts
  }
;;
