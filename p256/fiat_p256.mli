type point

val point_of_hex : Hex.t -> point option

val point_of_cs : Cstruct.t -> point option

val point_to_cs : point -> Cstruct.t

type scalar

val scalar_of_hex : Hex.t -> scalar option

val scalar_of_cs : Cstruct.t -> scalar option

val dh : scalar:scalar -> point:point -> Cstruct.t

val public : scalar -> point
