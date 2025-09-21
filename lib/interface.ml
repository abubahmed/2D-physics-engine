open! Core
open! Objects

type t =
  { height : int
  ; width : int
  ; panel : Panel.t
  ; canvas : Canvas.t
  }

let create ~height ~width =
  { height
  ; width
  ; panel = Panel.create ~height ~width:(width / 3)
  ; canvas = Canvas.create ~height ~width:(2 * width / 3)
  }
;;
