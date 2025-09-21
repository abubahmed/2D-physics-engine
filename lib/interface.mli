open! Core
open! Objects

type t =
  { height : int
  ; width : int
  ; panel : Panel.t
  ; canvas : Canvas.t
  }

val create : height:int -> width:int -> t
