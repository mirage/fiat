type point_error =
  [ `InvalidFormat
  | `InvalidLength
  | `InvalidRange
  | `NotOnCurve
  | `AtInfinity ]

type scalar_error =
  [ `InvalidLength
  | `InvalidRange ]

let error_to_string = function
  | `InvalidFormat ->
      "invalid format"
  | `NotOnCurve ->
      "point is not on curve"
  | `AtInfinity ->
      "point is at infinity"
  | `InvalidLength ->
      "invalid length"
  | `InvalidRange ->
      "invalid range"

let pp_point_error fmt e =
  Format.fprintf fmt "Cannot parse point: %s" (error_to_string e)

let pp_scalar_error fmt e =
  Format.fprintf fmt "Cannot parse scalar: %s" (error_to_string e)
