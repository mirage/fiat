type t

val of_hex : Hex.t -> t option

val of_cstruct : Cstruct.t -> t option

val of_hex_exn : Hex.t -> t

val bit_at : t -> int -> bool
