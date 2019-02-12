type point

val point_of_hex : Hex.t -> point option

type scalar

val scalar_of_hex : Hex.t -> scalar option

val dh : scalar:scalar -> point:point -> Cstruct.t

val public : scalar -> point
