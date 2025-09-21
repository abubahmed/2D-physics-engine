open! Core
open! Graphics

module TextBox : sig
  type t =
    { height : int
    ; width : int
    ; position : Vector.Plain.t
    ; id : string
    ; mutable display_text : string
    }

  val create
    :  height:int
    -> width:int
    -> position:Vector.Plain.t
    -> id:string
    -> display_text:string
    -> t

  val update_text : t -> display_text:string -> unit
end

module Button : sig
  type t =
    { height : int
    ; width : int
    ; position : Vector.Plain.t
    ; id : string
    ; mutable display_text : string
    ; mutable color : Graphics.color
    }

  val create
    :  height:int
    -> width:int
    -> position:Vector.Plain.t
    -> id:string
    -> display_text:string
    -> color:Graphics.color
    -> t

  val in_bounds : t -> int -> int -> bool
end

type t =
  { height : int
  ; width : int
  ; buttons : Button.t list
  ; text_boxes : TextBox.t list
  }

val find_button : t -> string -> Button.t option
val find_textbox : t -> string -> TextBox.t option
val create : height:int -> width:int -> t
