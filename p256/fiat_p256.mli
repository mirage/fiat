(** A point on the P-256 curve (public key), that is not the point at infinity. *)
type point

(** The type for point parsing errors. *)
type point_error =
  [ `Invalid_range
  | `Invalid_format
  | `Invalid_length
  | `Not_on_curve
  | `At_infinity ]

val pp_point_error : Format.formatter -> point_error -> unit
(** Pretty printer for point parsing errors *)

val point_of_cs : Cstruct.t -> (point, point_error) result
(** Convert from cstruct. Refuses the point at infinity.
    The format is the uncompressed format described in
    SEC1, section 2.3.4, that is to say:

    - the point at infinity is the single byte "00".
    - for a point (x, y) not at infinity, the format is:
      - the byte "04"
      - x serialized in big endian format, padded to 32 bytes.
      - y serialized in big endian format, padded to 32 bytes.

    @see <http://www.secg.org/sec1-v2.pdf>
*)

val point_of_hex : Hex.t -> (point, point_error) result
(** Convert a point from hex. See [point_of_cs]. *)

val point_to_cs : point -> Cstruct.t
(** Convert a point to a cstruct. See [point_of_cs] for the format. *)

(** A scalar value. *)
type scalar

(** The type for scalar parsing errors. *)
type scalar_error =
  [ `Invalid_length
  | `Invalid_range ]

val pp_scalar_error : Format.formatter -> scalar_error -> unit
(** Pretty printer for scalar parsing errors *)

val scalar_of_cs : Cstruct.t -> (scalar, scalar_error) result
(** Read data from a cstruct.
    It should be 32 bytes long, in big endian format. Returns an error when the
    number is zero, or if it is larger than or equal to the group order. *)

val scalar_of_hex : Hex.t -> (scalar, scalar_error) result
(** Like [scalar_of_cs] but read from hex data. *)

val dh : scalar:scalar -> point:point -> Cstruct.t
(** Perform Diffie-Hellman key exchange. This returns the x component of the
    scalar multiplication of [point] and [scalar]. *)

val public : scalar -> point
(** Compute the public key corresponding to a given private key. Internally,
    this multiplies the generator by the scalar.
    Given the invariant on [scalar], the result can't be the point at infinity. *)

val generate_key : rng:(int -> Cstruct.t) -> scalar * Cstruct.t

val key_exchange :
  private_key:scalar -> Cstruct.t -> (Cstruct.t, point_error) result
