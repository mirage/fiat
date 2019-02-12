type t = Cstruct.buffer

let create () = Cstruct.to_bigarray (Cstruct.create 32)

external mul : t -> t -> t -> unit = "fiat_p256_caml_mul" [@@noalloc]

let r_squared =
  Cstruct.to_bigarray
    (Cstruct.of_hex
       "0300000000000000fffffffffbfffffffefffffffffffffffdffffff04000000")

let to_montgomery x = mul x x r_squared

let copy dst src = Bigarray.Array1.blit src dst

external cmovznz : t -> bool -> t -> t -> unit = "fiat_p256_caml_cmovznz"
  [@@noalloc]

external from_bytes_buf :
  t -> Cstruct.buffer -> unit
  = "fiat_p256_caml_from_bytes"
  [@@noalloc]

let checked_buffer cs =
  assert (Cstruct.len cs = 32);
  Cstruct.to_bigarray cs

let from_bytes fe cs = from_bytes_buf fe (checked_buffer cs)

let one_bytes =
  Cstruct.of_hex
    "010000000000000000000000fffffffffffffffffffffffffeffffff00000000"

let one () =
  let fe = create () in
  from_bytes fe one_bytes; fe

external nz : t -> bool = "fiat_p256_caml_nz" [@@noalloc]

external sqr : t -> t -> unit = "fiat_p256_caml_sqr"

external from_montgomery : t -> unit = "fiat_p256_caml_from_montgomery"
  [@@noalloc]

external to_bytes_buf :
  Cstruct.buffer -> t -> unit
  = "fiat_p256_caml_to_bytes"
  [@@noalloc]

let to_bytes cs fe = to_bytes_buf (checked_buffer cs) fe

external add : t -> t -> t -> unit = "fiat_p256_caml_add" [@@noalloc]

external sub : t -> t -> t -> unit = "fiat_p256_caml_sub" [@@noalloc]

(**
  fe_inv calculates |out| = |in|^{-1}

  Based on Fermat's Little Theorem:
    a^p = a (mod p)
    a^{p-1} = 1 (mod p)
    a^{p-2} = a^{-1} (mod p)
     *)
let inv out in_ =
  let ftmp = create () in
  let ftmp2 = create () in
  (* each e_I will hold |in|^{2^I - 1} *)
  let e2 = create () in
  let e4 = create () in
  let e8 = create () in
  let e16 = create () in
  let e32 = create () in
  let e64 = create () in
  sqr ftmp in_ (* 2^1 *);
  mul ftmp in_ ftmp (* 2^2 - 2^0 *);
  copy e2 ftmp;
  sqr ftmp ftmp (* 2^3 - 2^1 *);
  sqr ftmp ftmp (* 2^4 - 2^2 *);
  mul ftmp ftmp e2 (* 2^4 - 2^0 *);
  copy e4 ftmp;
  sqr ftmp ftmp (* 2^5 - 2^1 *);
  sqr ftmp ftmp (* 2^6 - 2^2 *);
  sqr ftmp ftmp (* 2^7 - 2^3 *);
  sqr ftmp ftmp (* 2^8 - 2^4 *);
  mul ftmp ftmp e4 (* 2^8 - 2^0 *);
  copy e8 ftmp;
  for _ = 1 to 8 do
    sqr ftmp ftmp
  done
  (* 2^16 - 2^8 *);
  mul ftmp ftmp e8 (* 2^16 - 2^0 *);
  copy e16 ftmp;
  for _ = 1 to 16 do
    sqr ftmp ftmp
  done
  (* 2^32 - 2^16 *);
  mul ftmp ftmp e16 (* 2^32 - 2^0 *);
  copy e32 ftmp;
  for _ = 1 to 32 do
    sqr ftmp ftmp
  done
  (* 2^64 - 2^32 *);
  copy e64 ftmp;
  mul ftmp ftmp in_ (* 2^64 - 2^32 + 2^0 *);
  for _ = 1 to 192 do
    sqr ftmp ftmp
  done
  (* 2^256 - 2^224 + 2^192 *);
  mul ftmp2 e64 e32 (* 2^64 - 2^0 *);
  for _ = 1 to 16 do
    sqr ftmp2 ftmp2
  done
  (* 2^80 - 2^16 *);
  mul ftmp2 ftmp2 e16 (* 2^80 - 2^0 *);
  for _ = 1 to 8 do
    sqr ftmp2 ftmp2
  done
  (* 2^88 - 2^8 *);
  mul ftmp2 ftmp2 e8 (* 2^88 - 2^0 *);
  for _ = 1 to 4 do
    sqr ftmp2 ftmp2
  done
  (* 2^92 - 2^4 *);
  mul ftmp2 ftmp2 e4 (* 2^92 - 2^0 *);
  sqr ftmp2 ftmp2 (* 2^93 - 2^1 *);
  sqr ftmp2 ftmp2 (* 2^94 - 2^2 *);
  mul ftmp2 ftmp2 e2 (* 2^94 - 2^0 *);
  sqr ftmp2 ftmp2 (* 2^95 - 2^1 *);
  sqr ftmp2 ftmp2 (* 2^96 - 2^2 *);
  mul ftmp2 ftmp2 in_ (* 2^96 - 3 *);
  (* 2^256 - 2^224 + 2^192 + 2^96 - 3 *)
  mul out ftmp2 ftmp
