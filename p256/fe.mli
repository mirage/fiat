type t

val create : unit -> t

val one : unit -> t

val to_montgomery : t -> unit

val mul : t -> t -> t -> unit

val copy : t -> t -> unit

val cmovznz : t -> bool -> t -> t -> unit

val from_bytes : t -> Cstruct.t -> unit

val nz : t -> bool

val sqr : t -> t -> unit

val from_montgomery : t -> unit

val to_bytes : Cstruct.t -> t -> unit

val add : t -> t -> t -> unit

val sub : t -> t -> t -> unit

val inv : t -> t -> unit
