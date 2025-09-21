open! Core
open! Async

module Ball : sig
  type t =
    { mutable center : Vector.t
    ; mutable velocity : Vector.t
    ; mass : float
    ; radius : float
    ; mutable forces : Force.t list
    }
  [@@deriving equal, sexp_of]

  val net_force : t -> Vector.t
  val update_pos : t -> float -> unit
  val update_vel : t -> float -> unit
  val set_vel : t -> Vector.t -> unit
  val add_vel : t -> Vector.t -> unit
  val set_pos : t -> Vector.t -> unit
  val add_pos : t -> Vector.t -> unit
  val add_force : t -> Force.t -> unit
  val remove_force : t -> Force.t -> unit
  val create : center:Vector.t -> radius:float -> t
end

module Box : sig
  type t =
    { mutable min : Vector.t
    ; mutable max : Vector.t
    ; mutable theta : float
    ; mutable velocity : Vector.t
    ; mutable forces : Force.t list
    ; mass : float
    }

  val net_force : t -> Vector.t
  val update_pos : t -> float -> unit
  val update_vel : t -> float -> unit
  val set_vel : t -> Vector.t -> unit
  val add_vel : t -> Vector.t -> unit
  val set_pos : t -> Vector.t -> unit
  val add_pos : t -> Vector.t -> unit
  val add_force : t -> Force.t -> unit
  val remove_force : t -> Force.t -> unit
  val create : min:Vector.t -> max:Vector.t -> t
end

module Line : sig
  type t =
    { mutable first_endp : Vector.t
    ; mutable second_endp : Vector.t
    }
  [@@deriving equal, sexp_of]

  val calc_slope : t -> float
  val length : t -> float
  val length_squared : t -> float
  val x_length : t -> float
  val y_length : t -> float
  val center : t -> Vector.t
  val create : first_endp:Vector.t -> second_endp:Vector.t -> t
end

module Cup : sig
  type t =
    { mutable min : Vector.t
    ; mutable max : Vector.t
    }

  val create : min:Vector.t -> max:Vector.t -> t
end

module ObjectTypeSelector : sig
  type t =
    | Ball
    | Line
    | Cup
    | Box

  val to_string : t -> string
  val equal : t -> t -> bool
end

module ObjectSelector : sig
  type t =
    | Ball of Ball.t
    | Line of Line.t
    | Cup of Cup.t
    | Box of Box.t

  val to_string : t -> string
end

val find_min_max : Vector.t -> Vector.t -> Vector.t * Vector.t
