type t = Scalar of Cstruct.t

let pp fmt (Scalar s) = Cstruct_util.pp_hex_le fmt s

let is_in_range cs =
  let zero = Cstruct.create 32 in
  let n = Hex.to_cstruct Parameters.n in
  Cstruct_util.compare_be cs zero > 0 && Cstruct_util.compare_be n cs > 0

let of_cstruct cs =
  if Cstruct.len cs <> 32 then Error `Invalid_length
  else if is_in_range cs then Ok (Scalar (Cstruct.rev cs))
  else Error `Invalid_range

let of_hex h =
  let cs = Hex.to_cstruct h in
  of_cstruct cs

let of_hex_exn h =
  match of_hex h with
  | Ok p ->
      p
  | Error e ->
      failwith (Format.asprintf "of_hex_exn: %a" Error.pp_scalar_error e)

let bit_at (Scalar s) i =
  let byte_num = i / 8 in
  let bit_num = i mod 8 in
  let byte = Cstruct.get_uint8 s byte_num in
  byte land (1 lsl bit_num) <> 0
