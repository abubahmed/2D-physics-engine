open! Core

type t =
  | Create_object_select_first of Objects.ObjectTypeSelector.t
  | Create_object_select_final of (Objects.ObjectTypeSelector.t * Vector.t)
  | Drag_current_object of Objects.ObjectSelector.t
  | Select_current_object of Objects.ObjectTypeSelector.t
  | Free_state

let equal (a : t) (b : t) : bool =
  match a, b with
  | Free_state, Free_state -> true
  | Create_object_select_first o1, Create_object_select_first o2 ->
    Objects.ObjectTypeSelector.equal o1 o2
  | Create_object_select_final (o1, v1), Create_object_select_final (o2, v2)
    ->
    Objects.ObjectTypeSelector.equal o1 o2
  | Drag_current_object s1, Drag_current_object s2 -> true
  | Select_current_object o1, Select_current_object o2 -> true
  | _, _ -> false
;;

let to_string (click_state : t) =
  match click_state with
  | Free_state -> ""
  | Create_object_select_first obj ->
    "Select first point (" ^ Objects.ObjectTypeSelector.to_string obj ^ ")"
  | Create_object_select_final (obj, vec) ->
    "Select final point (" ^ Objects.ObjectTypeSelector.to_string obj ^ ")"
  | Drag_current_object selector ->
    "Dragging object (" ^ Objects.ObjectSelector.to_string selector ^ ")"
  | Select_current_object obj ->
    "Selecting object (" ^ Objects.ObjectTypeSelector.to_string obj ^ ")"
;;
