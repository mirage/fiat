(** The type for point parsing errors. *)
type point_error =
  [ `Invalid_range
  | `Invalid_format
  | `Invalid_length
  | `Not_on_curve
  | `At_infinity ]

val pp_point_error : Format.formatter -> point_error -> unit
(** Pretty printer for point parsing errors *)

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

module Dhe : sig
  val gen_key : rng:(int -> Cstruct.t) -> scalar * Cstruct.t
  (** [gen_key ~rng] generates a private and a public key for Ephemeral Diffie-Hellman.
      The returned key pair MUST only be used for a single key exchange.
      [rng] is the function used to repeteadly generate a private key until a valid candidate
      is obtained. [rng]'s int parameter is the size of the [Cstruct.t] to generate.
      The generated private key is checked to be greater than zero and lower than the group
      order meaning the public key cannot be the point at inifinity. *)

  val key_exchange :
    private_key:scalar -> Cstruct.t -> (Cstruct.t, point_error) result
  (** [key_exchange ~private_key received_public_key] performs Diffie-Hellman key exchange
      using your private key and the other party's public key. Returns the shared secret
      or an error if the received public is invalid or is the point at infinity.
      @see <http://www.secg.org/sec1-v2.pdf> for public key encoding format. *)
end
