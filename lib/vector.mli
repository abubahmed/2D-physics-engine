open! Core
open! Async

type t =
  { x : float
  ; y : float
  }
[@@deriving compare, equal, sexp_of]

val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> float -> t
val ( / ) : t -> float -> t
val multi_sum : t list -> t
val dist_squared : t -> t -> float
val dist : t -> t -> float
val dot_product : t -> t -> float
val scale : t -> k:float -> t
val mag_squared : t -> float
val mag : t -> float
val angle_between : t -> t -> float
val normalize : t -> t
val is_zero : t -> bool
val is_orthogonal : t -> t -> bool
val rotate : t -> theta:float -> t
val translate_x : t -> float -> t
val translate_y : t -> float -> t
val translate_xy : t -> float -> float -> t
val zero : unit -> t
val to_string : t -> string

module Plain : sig
  type t =
    { x : int
    ; y : int
    }
  [@@deriving compare, equal, sexp_of]
end
