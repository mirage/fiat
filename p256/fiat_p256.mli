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
(** [key_exchange ~private_key received_public_key] performs Diffie-Hellman key exchange
    using your private key and the other party's public key. Returns the shared secret
    or an error if the received public is invalid or is the point at infinity.

    @see <http://www.secg.org/sec1-v2.pdf> for public key encoding format. *)

(**/**)

(* Undocumented section *)

(** The type for secret parsing errors. *)
type secret_error =
  [ `Invalid_length
  | `Invalid_range ]

val pp_secret_error : Format.formatter -> secret_error -> unit
(** Pretty printer for secret parsing errors *)

val secret_of_cs : Cstruct.t -> (secret, secret_error) result
(** Read a secret from a cstruct.
    It should be 32 bytes long, in big endian format. Returns an error when the
    number is zero, or if it is larger than or equal to the group order. *)

(**/**)
