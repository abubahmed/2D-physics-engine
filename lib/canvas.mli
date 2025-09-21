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

  val create : height:int -> width:int -> t
  val in_bounds : t -> int -> int -> bool
  val bound_objects : t -> unit
  val add_ball : t -> Ball.t -> unit
  val add_line : t -> Line.t -> unit
  val add_box : t -> Box.t -> unit
  val add_cup : t -> Cup.t -> unit
  val clear : t -> unit
