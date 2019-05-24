val pp_hex_le : Format.formatter -> Cstruct.t -> unit
(** Display the contents of a cstruct as hex data, seen as a little endian
    number. *)

val compare_be_variable_time : Cstruct.t -> Cstruct.t -> int
(** Compare two cstructs, interpreting them as big endian numbers.

    This function will take a different time depending on when the difference
    appears. This is not a security concern when either of the operands is
    public, in particular for range checks.

    Raises [Invalid_argument _] if they have a different length. *)
