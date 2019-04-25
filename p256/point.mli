(** A Point on the P-256 curve.
    It is backed by [Fe.t], and as such, is mutable. *)
type t

type error = [
  `CoordinateTooLarge
  | `InvalidFormat
  | `InvalidLength
  | `NotOnCurve
]

val at_infinity : unit -> t
(** The point at infinity *)

val add : t -> t -> t
(** Point addition. [add dst p q] adds [p] and [q], and stores the result
    into [dst]. *)

val double : t -> t
(** Point doubling. [double dst p q] doubles [p], and stores the result
    into [dst]. *)

val of_cstruct : Cstruct.t -> (t, error) result
(** Convert from cstruct. The format is the uncompressed format described in
    SEC1, section 2.3.4, that is to say:

    - the point at infinity is the single byte "00".
    - for a point (x, y) not at infinity, the format is:
      - the byte "04"
      - x serialized in big endian format, padded to 32 bytes.
      - y serialized in big endian format, padded to 32 bytes.

    @see <http://www.secg.org/sec1-v2.pdf>
*)

val of_hex : Hex.t -> (t, error) result
(** Convert from hex. See [of_cstruct]. *)

val of_hex_exn : Hex.t -> t
(** Convert from hex, raising an exception if data is invalid. See [of_hex]. *)

val to_cstruct : t -> Cstruct.t
(** Convert to a cstruct. See [of_cstruct] for the format. *)

val pp : Format.formatter -> t -> unit
(** Display a point in the format documented in [of_cstruct]. *)

val x_of_finite_point : t -> Cstruct.t
(** Return only the X coordinate of a point that is not at infinity. *)
