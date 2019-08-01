type t
(** A scalar value strictly between 1 and n-1 where n is the group order. *)

val of_cstruct : Cstruct.t -> (t, Error.scalar_error) result
(** Read data from a cstruct.
    It should be 32 bytes long, in big endian format. Returns an error when the
    number is zero, or if it is larger than or equal to the group order. *)

val bit_at : t -> int -> bool
(** [bit_at d n] returns the [n]th bit from [d], where bit 0 is the least
    significant bit. *)
