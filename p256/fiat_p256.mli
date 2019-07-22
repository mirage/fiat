(** The type for public key parsing errors. *)
type error =
  [ `Invalid_range
  | `Invalid_format
  | `Invalid_length
  | `Not_on_curve
  | `At_infinity ]

val pp_error : Format.formatter -> error -> unit
(** Pretty printer for public key parsing errors *)

(** Type for P256 private keys *)
type secret

val gen_key : rng:(int -> Cstruct.t) -> secret * Cstruct.t
(** [gen_key ~rng] generates a private and a public key for Ephemeral Diffie-Hellman
    over P256. The returned key pair MUST only be used for a single key exchange.

    [rng] is the function used to repeteadly generate a private key until a valid candidate
    is obtained. [rng]'s int parameter is the size of the [Cstruct.t] to generate.
    If [rng] returns an invalid length buffer, [Failure _] is raised.

    The generated private key is checked to be greater than zero and lower than the group
    order meaning the public key cannot be the point at inifinity. *)

val key_exchange : secret -> Cstruct.t -> (Cstruct.t, error) result
(** [key_exchange secret received_public_key] performs Diffie-Hellman key exchange
    using your secret and the data received from the other party. Returns the shared secret
    or an error if the received data is wrongly encoded, doesn't represent a point on the curve
    or represent the point at infinity.

    The shared secret is returned as is i.e. not stripped from leading 0x00 bytes.

    @see <http://www.secg.org/sec1-v2.pdf> for public key encoding format. *)

(**/**)

module For_tests : sig
  module Cstruct_util : sig
    val compare_be : Cstruct.t -> Cstruct.t -> int

    val pp_hex_le : Format.formatter -> Cstruct.t -> unit
  end

  module Parameters : sig
    val p : Hex.t

    val n : Hex.t
  end

  module Point : sig
    type t

    val validate_finite_point :
      x:Cstruct.t -> y:Cstruct.t -> (t, error) result

    val of_hex : Hex.t -> (t, error) result

    val of_hex_exn : Hex.t -> t

    val pp : Format.formatter -> t -> unit

    val double : t -> t

    val add : t -> t -> t

    val at_infinity : unit -> t
  end

  module Error : sig
    type scalar_error

    val pp_scalar_error : Format.formatter -> scalar_error -> unit
  end

  module Scalar : sig
    type t

    val of_hex : Hex.t -> (t, Error.scalar_error) result

    val of_hex_exn : Hex.t -> t

    val pp : Format.formatter -> t -> unit
  end

  module Montgomery_ladder : sig
    val scalar_mult : Scalar.t -> Point.t -> Point.t
  end

  val public : Scalar.t -> Point.t

  val dh : scalar:Scalar.t -> point:Point.t -> Cstruct.t
end
