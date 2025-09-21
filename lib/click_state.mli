open! Core

type t =
  | Create_object_select_first of Objects.ObjectTypeSelector.t
  | Create_object_select_final of (Objects.ObjectTypeSelector.t * Vector.t)
  | Drag_current_object of Objects.ObjectSelector.t
  | Select_current_object of Objects.ObjectTypeSelector.t
  | Free_state

val equal : t -> t -> bool
val to_string : t -> string
