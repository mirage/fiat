type t = Scalar of Cstruct.t

let pp fmt (Scalar s) = Cstruct_util.pp_hex_le fmt s

let strip_leading_zeroes cs =
  let first_nonzero_index = ref None in
  let cs_len = Cstruct.len cs in
  for i = cs_len - 1 downto 0 do
    if Cstruct.get_uint8 cs i <> 0 then first_nonzero_index := Some i
  done;
  match !first_nonzero_index with
  | None ->
      Cstruct.empty
  | Some i ->
      let off = i in
      let len = cs_len - i in
      Cstruct.sub cs off len

let pad ~total_len cs =
  match total_len - Cstruct.len cs with
  | 0 ->
      Some cs
  | n
    when n < 0 ->
      None
  | pad_len ->
      Some (Cstruct.append cs (Cstruct.create pad_len))

let is_in_range cs_le =
  let zero = Cstruct.create 32 in
  let n = Hex.to_cstruct Parameters.n in
  let cs = Cstruct.rev cs_le in
  Cstruct_util.compare_be cs zero > 0 && Cstruct_util.compare_be n cs > 0

let of_cstruct cs =
  let stripped = strip_leading_zeroes cs in
  match pad ~total_len:32 (Cstruct.rev stripped) with
  | Some padded
    when is_in_range padded ->
      Some (Scalar padded)
  | _ ->
      None

let of_hex h =
  let cs = Hex.to_cstruct h in
  of_cstruct cs

let pp_opt pp fmt = function
  | None ->
      Format.fprintf fmt "None"
  | Some x ->
      pp fmt x

let%expect_test "of_hex" =
  let test h =
    let s = of_hex h in
    Format.printf "%a\n" (pp_opt pp) s
  in
  test
    (`Hex "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f");
  [%expect
    {| 000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f
|}];
  test (`Hex "03");
  [%expect
    {| 0000000000000000000000000000000000000000000000000000000000000003 |}];
  test
    (`Hex
      "00000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f");
  [%expect
    {| 000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f |}];
  test (`Hex "");
  [%expect {| None |}];
  test
    (`Hex
      "2000000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f");
  [%expect {| None |}];
  test
    (`Hex "0000000000000000000000000000000000000000000000000000000000000000");
  [%expect {| None |}];
  test
    (`Hex
      (* n-1 *)
      "FFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632550");
  [%expect
    {| ffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632550 |}];
  test Parameters.n;
  [%expect {| None |}];
  test
    (`Hex
      (* n+1 *)
      "FFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632552");
  [%expect {| None |}]

let of_hex_exn h =
  match of_hex h with
  | Some p ->
      p
  | None ->
      failwith "of_hex_exn"

let bit_at (Scalar s) i =
  let byte_num = i / 8 in
  let bit_num = i mod 8 in
  let byte = Cstruct.get_uint8 s byte_num in
  byte land (1 lsl bit_num) <> 0
