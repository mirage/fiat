let linkme = ()

module Cstruct_util = struct
  open Fiat_p256.For_tests.Cstruct_util

  let%expect_test "compare_be" =
    let test a b = print_int (compare_be a b) in
    test (Cstruct.of_string "aa") (Cstruct.of_string "ab");
    [%expect {| -1 |}];
    test (Cstruct.of_string "ab") (Cstruct.of_string "aa");
    [%expect {| 1 |}];
    test (Cstruct.of_string "aa") (Cstruct.of_string "aa");
    [%expect {| 0 |}];
    test (Cstruct.of_string "abx") (Cstruct.of_string "aaz");
    [%expect {| 1 |}];
    ()
end

module Point = struct
  open Fiat_p256.For_tests.Point

  let%expect_test "validate_finite_point" =
    let is_ok = function
      | Ok _ -> true
      | Error _ -> false
    in
    let test ~x ~y =
      Printf.printf "%b"
        (is_ok
           (validate_finite_point ~x:(Hex.to_cstruct x) ~y:(Hex.to_cstruct y)))
    in
    test
      ~x:
        (`Hex
          "62d5bd3372af75fe85a040715d0f502428e07046868b0bfdfa61d731afe44f26")
      ~y:
        (`Hex
          "ac333a93a9e70a81cd5a95b5bf8d13990eb741c8c38872b4a07d275a014e30cf");
    [%expect {| true |}];
    test
      ~x:
        (`Hex
          "0000000000000000000000000000000000000000000000000000000000000000")
      ~y:
        (`Hex
          "0000000000000000000000000000000000000000000000000000000000000000");
    [%expect {| false |}];
    let zero = `Hex (String.make 64 '0') in
    let sb =
      `Hex "66485c780e2f83d72433bd5d84a06bb6541c2af31dae871728bf856a174f93f4"
    in
    test ~x:zero ~y:sb;
    [%expect {| true |}];
    test ~x:Fiat_p256.For_tests.Parameters.p ~y:sb;
    [%expect {| false |}]

  let%expect_test "of_hex" =
    let test hex =
      let ok =
        match of_hex hex with
        | Ok _ -> true
        | Error _ -> false
      in
      Printf.printf "%b" ok
    in
    test (`Hex "00");
    [%expect {| true |}];
    test (`Hex "0001");
    [%expect {| false |}];
    test
      (`Hex
        "0400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
    [%expect {| false |}];
    test
      (`Hex
        "0462d5bd3372af75fe85a040715d0f502428e07046868b0bfdfa61d731afe44f26ac333a93a9e70a81cd5a95b5bf8d13990eb741c8c38872b4a07d275a014e30cf");
    [%expect {| true |}];
    test (`Hex "0400");
    [%expect {| false |}];
    test (`Hex "ff");
    [%expect {| false |}]
end

let key_pair_of_hex h = Fiat_p256.gen_key ~rng:(fun _ -> Hex.to_cstruct h)

let scalar_of_hex h = fst (key_pair_of_hex h)

let pp_hex_le fmt cs =
  let n = Cstruct.len cs in
  for i = n - 1 downto 0 do
    let byte = Cstruct.get_uint8 cs i in
    Format.fprintf fmt "%02x" byte
  done

let pp_result ppf = function
  | Ok cs -> pp_hex_le ppf cs
  | Error e -> Format.fprintf ppf "%a" Fiat_p256.pp_error e

let%expect_test "key_exchange" =
  let test d p =
    Format.printf "%a\n" pp_result (Fiat_p256.key_exchange d p)
  in
  let d_a, p_a =
    key_pair_of_hex
      (`Hex
        "200102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f")
  in
  let d_b, p_b =
    key_pair_of_hex
      (`Hex
        "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f")
  in
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
    {| a7666bcc3818472194460f7df22d80a5886da0e1679eac930175ce1ff733c7ca |}];
  let test_h scalar point =
    let scalar = scalar_of_hex scalar in
    let point = Hex.to_cstruct point in
    let res = Fiat_p256.key_exchange scalar point in
    Format.printf "%a\n" pp_result res
  in
  let point =
    `Hex
      "046B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0F4A13945D898C2964FE342E2FE1A7F9B8EE7EB4A7C0F9E162BCE33576B315ECECBB6406837BF51F5"
  in
  test_h
    (`Hex "0000000000000000000000000000000000000000000000000000000000000001")
    point;
  [%expect
    {| 96c298d84539a1f4a033eb2d817d0377f240a463e5e6bcf847422ce1f2d1176b |}];
  test_h
    (`Hex "0000000000000000000000000000000000000000000000000000000000000002")
    point;
  [%expect
    {| 78996647fc480ba6351bf277e26989c0c31ab5040338528a7e4f038d187bf27c |}];
  test_h
    (`Hex "0000000000000000000000000000000000000000000000000000000000000004")
    point;
  [%expect
    {| 5208036b44029350ef965578dbe21f03d02be69e65de2da0bb8fd032354a53e2 |}];
  test_h
    (`Hex "0612465c89a023ab17855b0a6bcebfd3febb53aef84138647b5352e02c10c346")
    (`Hex
      "0462d5bd3372af75fe85a040715d0f502428e07046868b0bfdfa61d731afe44f26ac333a93a9e70a81cd5a95b5bf8d13990eb741c8c38872b4a07d275a014e30cf");
  [%expect
    {| 854271e19508bc935ab22b95cd2be13a0e78265f528b658b3219028b900d0253 |}];
  test_h
    (`Hex "0a0d622a47e48f6bc1038ace438c6f528aa00ad2bd1da5f13ee46bf5f633d71a")
    (`Hex
      "043cbc1b31b43f17dc200dd70c2944c04c6cb1b082820c234a300b05b7763844c74fde0a4ef93887469793270eb2ff148287da9265b0334f9e2609aac16e8ad503");
  [%expect
    {| ffffffffffffffffffffffffffffffff3022cfeeffffffffffffffffffffff7f |}];
  test_h
    (`Hex "55d55f11bb8da1ea318bca7266f0376662441ea87270aa2077f1b770c4854a48")
    (`Hex
      "04000000000000000000000000000000000000000000000000000000000000000066485c780e2f83d72433bd5d84a06bb6541c2af31dae871728bf856a174f93f4");
  [%expect
    {| 48e82c9b82c88cb9fc2a5cff9e7c41bc4255ff6bd3814538c9b130877c07e4cf |}]
