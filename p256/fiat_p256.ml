let dh ~scalar ~point =
  Point.x_of_finite_point (Montgomery_ladder.scalar_mult scalar point)

let base_point = Point.of_hex_exn Parameters.g

let public scalar = Montgomery_ladder.scalar_mult scalar base_point

let%expect_test "dh" =
  let test d p =
    Format.printf "%a\n" Cstruct_util.pp_hex_le (dh ~scalar:d ~point:p)
  in
  let d_a =
    Scalar.of_hex_exn
      (`Hex
        "200102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f")
  in
  let d_b =
    Scalar.of_hex_exn
      (`Hex
        "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f")
  in
  let p_a = public d_a in
  let p_b = public d_b in
  test d_b p_a;
  [%expect
    {| 2e3e4065a62a7f425aaf8aae3d158f367c733300b5002e0b62f4bc6260789e1b |}];
  test d_a p_b;
  [%expect
    {| 2e3e4065a62a7f425aaf8aae3d158f367c733300b5002e0b62f4bc6260789e1b |}];
  test d_a p_a;
  [%expect
    {| 2ea4e810837da217a5bfd05f01d12459eeda830b6e0dec7f8afa425c5b55c507 |}];
  test d_b p_b;
  [%expect
    {| a7666bcc3818472194460f7df22d80a5886da0e1679eac930175ce1ff733c7ca |}]

type point = Point.t

let point_of_hex = Point.of_hex

let point_of_cs = Point.of_cstruct

let point_to_cs = Point.to_cstruct

type scalar = Scalar.t

let scalar_of_hex = Scalar.of_hex

let scalar_of_cs = Scalar.of_cstruct
