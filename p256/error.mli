type point_error =
  [ `InvalidFormat
  | `InvalidRange
  | `InvalidLength
  | `NotOnCurve
  | `AtInfinity ]

type scalar_error =
  [ `InvalidLength
  | `InvalidRange ]

val pp_point_error : Format.formatter -> point_error -> unit

val pp_scalar_error : Format.formatter -> scalar_error -> unit
