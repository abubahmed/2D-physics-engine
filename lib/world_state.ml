open! Core
open! Async

type t =
  | In_progress
  | Paused
  | Clear
  | Failure
[@@deriving sexp_of, compare]

let to_string t =
  match t with
  | In_progress -> "in-progress"
  | Paused -> "paused"
  | Clear -> "clear"
  | Failure -> "failure"
;;

let equal (a : t) (b : t) : bool =
  match a, b with
  | In_progress, In_progress
  | Paused, Paused
  | Clear, Clear
  | Failure, Failure ->
    true
  | _ -> false
;;
