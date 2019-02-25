val pp_hex_le : Format.formatter -> Cstruct.t -> unit
(** Display the contents of a cstruct as hex data, seen as a little endian
    number. *)

val rev : Cstruct.t -> Cstruct.t
(** [rev t] is [t] in reverse order. The return value is a freshly allocated
    cstruct, and the argument is not modified. *)
