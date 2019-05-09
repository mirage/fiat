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
(** Convert from a cstruct encoding a point in the uncompressed format
    described in {{http://www.secg.org/sec1-v2.pdf}SEC1}, section 2.3.4.

    This performs the following checks:
    - The header is either `00` or `04`, otherwise returns [Error `Invalid_format].
    - The point is not at infinity (header `OO`), otherwise returns [Error `At_infinity].
    - The point coordinate are 32 bytes long, otherwise returns [Error `Invalid_length].
    - The point coordinates are within 0 and p-1, where p is the curve field order,
      otherwise returns [Error `Invalid_range].
    - The point is on the curve, otherwise returns [Error `Not_on_curve].
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
(** Converts from a 32 bytes big endian cstruct.

    This performs the following checks:
    - The scalar is 32 bytes long, otherwise returns [Error `Invalid_length].
    - The scalar is within 1 and n-1, where n is the curve group order, otherwise
      returns [Error `Invalid_range]. *)

val scalar_of_hex : Hex.t -> (scalar, scalar_error) result
(** Like [scalar_of_cs] but read from hex data. *)

val dh : scalar:scalar -> point:point -> Cstruct.t
(** Perform Diffie-Hellman key exchange. This returns the x component of the
    scalar multiplication of [point] and [scalar]. *)

val public : scalar -> point
(** Compute the p256 public key corresponding to a given private key.
    Given the invariant on [scalar], the result can't be the point at infinity. *)
