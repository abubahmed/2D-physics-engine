let to_pixels units = Int.of_float (units *. Constants.pixels_per_unit)
let to_units pixels = Float.of_int pixels /. Constants.pixels_per_unit

let to_pixels_vector (vector : Vector.t) : Vector.Plain.t =
  { x = to_pixels vector.x; y = to_pixels vector.y }
;;

let to_units_vector (vector : Vector.Plain.t) : Vector.t =
  { x = to_units vector.x; y = to_units vector.y }
;;
