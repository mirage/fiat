(** A scalar value. *)
type t

val of_cstruct : Cstruct.t -> t option
(** Read data from a cstruct.
    It should be 32 bytes long, in big endian format. *)

val of_hex : Hex.t -> t option
(** Like [of_cstruct] but read from hex data. *)

val of_hex_exn : Hex.t -> t
(** Like [of_hex] but raises if there is an error. *)

val bit_at : t -> int -> bool
(** [bit_at d n] returns the [n]th bit from [d], where bit 0 is the least
    significant bit. *)
