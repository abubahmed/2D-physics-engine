open! Core
open! Async

module Ball = struct
  type t =
    { mutable center : Vector.t
    ; mutable velocity : Vector.t
    ; mass : float
    ; radius : float
    ; mutable forces : Force.t list
    }
  [@@deriving equal, sexp_of]

  let net_force t =
    List.fold t.forces ~init:(Vector.zero ()) ~f:(fun init force ->
      Vector.( + ) init force.vector)
  ;;

  let update_pos t (dt : float) =
    let dx = Vector.( * ) t.velocity dt in
    let new_position = Vector.( + ) t.center dx in
    t.center <- new_position
  ;;

  let update_vel t (dt : float) =
    let acceleration = Vector.( / ) (net_force t) t.mass in
    let dv = Vector.( * ) acceleration dt in
    let new_velocity = Vector.( + ) t.velocity dv in
    t.velocity <- new_velocity
  ;;

  let set_vel t (nv : Vector.t) = t.velocity <- nv
  let add_vel t (dv : Vector.t) = t.velocity <- Vector.( + ) t.velocity dv
  let set_pos t (np : Vector.t) = t.center <- np
  let add_pos t (dx : Vector.t) = t.center <- Vector.( + ) t.center dx
  let add_force t (force : Force.t) = t.forces <- [ force ] @ t.forces

  let remove_force t (force : Force.t) =
    t.forces
    <- List.filter t.forces ~f:(fun other_force ->
         not (Force.equal force other_force))
  ;;

  let create ~center ~radius =
    let area = Float.pi *. radius *. radius in
    let mass = area *. Constants.surface_density in
    let gravity = Vector.scale { x = 0.0; y = Constants.g } ~k:mass in
    { center
    ; mass
    ; radius
    ; velocity = Vector.zero ()
    ; forces = [ { vector = gravity; name = "gravity" } ]
    }
  ;;
end

module Box = struct
  type t =
    { mutable min : Vector.t
    ; mutable max : Vector.t
    ; mutable theta : float
    ; mutable velocity : Vector.t
    ; mutable forces : Force.t list
    ; mass : float
    }

  let net_force t =
    List.fold t.forces ~init:(Vector.zero ()) ~f:(fun init force ->
      Vector.( + ) init force.vector)
  ;;

  let update_pos t (dt : float) =
    let dx = Vector.( * ) t.velocity dt in
    let new_min_pos = Vector.( + ) t.min dx in
    let new_max_pos = Vector.( + ) t.max dx in
    t.min <- new_min_pos;
    t.max <- new_max_pos
  ;;

  let update_vel t (dt : float) =
    let acceleration = Vector.( / ) (net_force t) t.mass in
    let dv = Vector.( * ) acceleration dt in
    let new_velocity = Vector.( + ) t.velocity dv in
    t.velocity <- new_velocity
  ;;

  let set_vel t (nv : Vector.t) = t.velocity <- nv
  let add_vel t (dv : Vector.t) = t.velocity <- Vector.( + ) t.velocity dv

  let set_pos t (np : Vector.t) =
    t.min <- np;
    t.max <- np
  ;;

  let add_pos t (dx : Vector.t) =
    t.min <- Vector.( + ) t.min dx;
    t.max <- Vector.( + ) t.max dx
  ;;

  let add_force t (force : Force.t) = t.forces <- [ force ] @ t.forces

  let remove_force t (force : Force.t) =
    t.forces
    <- List.filter t.forces ~f:(fun other_force ->
         not (Force.equal force other_force))
  ;;

  let create ~(min : Vector.t) ~(max : Vector.t) =
    let area = (max.x -. min.x) *. (max.y -. min.y) in
    let mass = area *. Constants.surface_density in
    { min; max; mass; theta = 0.0; velocity = Vector.zero (); forces = [] }
  ;;
end

module Line = struct
  type t =
    { mutable first_endp : Vector.t
    ; mutable second_endp : Vector.t
    }
  [@@deriving equal, sexp_of]

  let calc_slope t =
    let ydiff = t.second_endp.y -. t.first_endp.y in
    let xdiff = t.second_endp.x -. t.first_endp.x in
    ydiff /. xdiff
  ;;

  let length t = Vector.dist t.first_endp t.second_endp
  let length_squared t = Vector.dist_squared t.first_endp t.second_endp
  let x_length t = Float.abs (t.first_endp.x -. t.second_endp.x)
  let y_length t = Float.abs (t.first_endp.y -. t.second_endp.y)

  let center t =
    let endp_sum = Vector.( + ) t.first_endp t.second_endp in
    Vector.( / ) endp_sum 2.0
  ;;

  let create ~first_endp ~second_endp = { first_endp; second_endp }
end

module Cup = struct
  type t =
    { mutable min : Vector.t
    ; mutable max : Vector.t
    }

  let create ~min ~max = { min; max }
end

module ObjectTypeSelector = struct
  type t =
    | Ball
    | Line
    | Cup
    | Box

  let to_string = function
    | Ball -> "Ball"
    | Line -> "Line"
    | Cup -> "Cup"
    | Box -> "Box"
  ;;

  let equal (a : t) (b : t) : bool =
    match a, b with
    | Ball, Ball | Line, Line | Cup, Cup | Box, Box -> true
    | _, _ -> false
  ;;
end

module ObjectSelector = struct
  type t =
    | Ball of Ball.t
    | Line of Line.t
    | Cup of Cup.t
    | Box of Box.t

  let to_string = function
    | Ball _ -> "Ball"
    | Line _ -> "Line"
    | Cup _ -> "Cup"
    | Box _ -> "Box"
  ;;
end

let find_min_max
      (first_selected_pos : Vector.t)
      (second_selected_pos : Vector.t)
  =
  let x1 = first_selected_pos.x in
  let x2 = second_selected_pos.x in
  let y1 = first_selected_pos.y in
  let y2 = second_selected_pos.y in
  let smaller_x = Float.min x1 x2 in
  let larger_x = Float.max x1 x2 in
  let smaller_y = Float.min y1 y2 in
  let larger_y = Float.max y1 y2 in
  let max_pos = { Vector.x = larger_x; Vector.y = larger_y } in
  let min_pos = { Vector.x = smaller_x; Vector.y = smaller_y } in
  min_pos, max_pos
;;
