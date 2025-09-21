open! Core

type t =
  { vector : Vector.t
  ; name : string
  }
[@@deriving equal, sexp_of]
