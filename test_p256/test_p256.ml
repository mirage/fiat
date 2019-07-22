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

  let%expect_test "double" =
    let print_point = Format.printf "%a\n" pp in
    let test p = print_point @@ double p in
    let p =
      of_hex_exn
        (`Hex
          "046B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0F4A13945D898C2964FE342E2FE1A7F9B8EE7EB4A7C0F9E162BCE33576B315ECECBB6406837BF51F5")
    in
    test p;
    [%expect
      {|
    047cf27b188d034f7e8a52380304b51ac3c08969e277f21b35a60b48fc4766997807775510db8ed040293d9ac69f7430dbba7dade63ce982299e04b79d227873d1 |}];
    test (at_infinity ());
    [%expect {| 00 |}];
    test
      (of_hex_exn
         (`Hex
           "046b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c296b01cbd1c01e58065711814b583f061e9d431cca994cea1313449bf97c840ae0a"));
    [%expect
      {|
    047cf27b188d034f7e8a52380304b51ac3c08969e277f21b35a60b48fc47669978f888aaee24712fc0d6c26539608bcf244582521ac3167dd661fb4862dd878c2e |}]

  let%expect_test "add" =
    let test p q =
      let r = add p q in
      Format.printf "%a\n" pp r
    in
    let p =
      of_hex_exn
        (`Hex
          "046B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0F4A13945D898C2964FE342E2FE1A7F9B8EE7EB4A7C0F9E162BCE33576B315ECECBB6406837BF51F5")
    in
    let q =
      of_hex_exn
        (`Hex
          "046b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c296b01cbd1c01e58065711814b583f061e9d431cca994cea1313449bf97c840ae0a")
    in
    test p (at_infinity ());
    [%expect
      {|
    046b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c2964fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5 |}];
    test (at_infinity ()) (at_infinity ());
    [%expect {| 00 |}];
    test p p;
    [%expect
      {|
    047cf27b188d034f7e8a52380304b51ac3c08969e277f21b35a60b48fc4766997807775510db8ed040293d9ac69f7430dbba7dade63ce982299e04b79d227873d1 |}];
    test p q;
    [%expect {| 00 |}];
    test q q;
    [%expect
      {|
    047cf27b188d034f7e8a52380304b51ac3c08969e277f21b35a60b48fc47669978f888aaee24712fc0d6c26539608bcf244582521ac3167dd661fb4862dd878c2e
|}];
    test
      (of_hex_exn
         (`Hex
           "04b719d9cc1c48ebe628eb717c7eb7f619350a4beec1338e31b6b99da50b8b3e1b95306f9fa3bb49fad08751f648f3a074c16ac4aa02bd057a8a22150c2b719b1d"))
      (of_hex_exn
         (`Hex
           "043f894c1ede3f1d6c6ae483e5d260afe006303d19d5c49e2595461bb91fd0ff96c0ac188423e2dd175dbb6e2fc449fd37cee52177764a21d24e74f84261491a7f"));
    [%expect
      "042e831e3b63a3d7c195f2fb4654b90e8cecd0d80b10448ff6699221e626a5095a95d666ef1f3114b49f6e4dac5f758a1ee81878164fd4e6e870e6ed5c0432e95b"];
    test
      (of_hex_exn
         (`Hex
           "042e831e3b63a3d7c195f2fb4654b90e8cecd0d80b10448ff6699221e626a5095a95d666ef1f3114b49f6e4dac5f758a1ee81878164fd4e6e870e6ed5c0432e95b"))
      (of_hex_exn
         (`Hex
           "041690393bb369ad609b7a43b25f11a5e62bcc298e8dde31ac5aa28c820cce7fb255f9c348aece766354b536211e307d02f38dda103b14a26796875444ef90d232"));
    [%expect
      "0453020d908b0219328b658b525f26780e3ae12bcd952bb25a93bc0895e1714285b2ba871dd1652c3f467df15c6b70647efbcbbab5cbf7f55e6ff336f843d628a1"]

  let%expect_test "double vs add" =
    let p =
      of_hex_exn
        (`Hex
          "046B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0F4A13945D898C2964FE342E2FE1A7F9B8EE7EB4A7C0F9E162BCE33576B315ECECBB6406837BF51F5")
    in
    let print_point = Format.printf "%a\n" pp in
    print_point @@ add p @@ add p @@ add p p;
    [%expect
      {| 04e2534a3532d08fbba02dde659ee62bd0031fe2db785596ef509302446b030852e0f1575a4c633cc719dfee5fda862d764efc96c3f30ee0055c42c23f184ed8c6 |}];
    print_point @@ double @@ double p;
    [%expect
      {| 04e2534a3532d08fbba02dde659ee62bd0031fe2db785596ef509302446b030852e0f1575a4c633cc719dfee5fda862d764efc96c3f30ee0055c42c23f184ed8c6 |}]
end

module Scalar = struct
  open Fiat_p256.For_tests.Scalar

  let pp_err pp fmt = function
    | Error e -> Fiat_p256.For_tests.Error.pp_scalar_error fmt e
    | Ok x -> pp fmt x

  let%expect_test "of_hex" =
    let test h =
      let s = of_hex h in
      Format.printf "%a\n" (pp_err pp) s
    in
    test
      (`Hex
        "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f");
    [%expect
      {| 000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f |}];
    test
      (`Hex
        "0000000000000000000000000000000000000000000000000000000000000003");
    [%expect
      {| 0000000000000000000000000000000000000000000000000000000000000003 |}];
    test
      (`Hex
        "2000000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f");
    [%expect {| Cannot parse scalar: invalid length |}];
    test
      (`Hex
        "0000000000000000000000000000000000000000000000000000000000000000");
    [%expect {| Cannot parse scalar: invalid range |}];
    test
      (`Hex
        (* n-1 *)
        "FFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632550");
    [%expect
      {| ffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632550 |}];
    test Fiat_p256.For_tests.Parameters.n;
    [%expect {| Cannot parse scalar: invalid range |}];
    test
      (`Hex
        (* n+1 *)
        "FFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632552");
    [%expect {| Cannot parse scalar: invalid range |}]
end

module Montgomery_ladder = struct
  open Fiat_p256.For_tests.Montgomery_ladder

  let%expect_test "scalar mult" =
    let test ~scalar ~point =
      let scalar = Fiat_p256.For_tests.Scalar.of_hex_exn scalar in
      let point = Fiat_p256.For_tests.Point.of_hex_exn point in
      let res = scalar_mult scalar point in
      Format.printf "%a\n" Fiat_p256.For_tests.Point.pp res
    in
    let point =
      `Hex
        "046B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0F4A13945D898C2964FE342E2FE1A7F9B8EE7EB4A7C0F9E162BCE33576B315ECECBB6406837BF51F5"
    in
    test
      ~scalar:
        (`Hex
          "0000000000000000000000000000000000000000000000000000000000000001")
      ~point;
    [%expect
      {| 046b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c2964fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5 |}];
    test
      ~scalar:
        (`Hex
          "0000000000000000000000000000000000000000000000000000000000000002")
      ~point;
    [%expect
      {| 047cf27b188d034f7e8a52380304b51ac3c08969e277f21b35a60b48fc4766997807775510db8ed040293d9ac69f7430dbba7dade63ce982299e04b79d227873d1 |}];
    test
      ~scalar:
        (`Hex
          "0000000000000000000000000000000000000000000000000000000000000004")
      ~point;
    [%expect
      {| 04e2534a3532d08fbba02dde659ee62bd0031fe2db785596ef509302446b030852e0f1575a4c633cc719dfee5fda862d764efc96c3f30ee0055c42c23f184ed8c6 |}];
    test
      ~scalar:
        (`Hex
          "0612465c89a023ab17855b0a6bcebfd3febb53aef84138647b5352e02c10c346")
      ~point:
        (`Hex
          "0462d5bd3372af75fe85a040715d0f502428e07046868b0bfdfa61d731afe44f26ac333a93a9e70a81cd5a95b5bf8d13990eb741c8c38872b4a07d275a014e30cf");
    [%expect
      {| 0453020d908b0219328b658b525f26780e3ae12bcd952bb25a93bc0895e1714285b2ba871dd1652c3f467df15c6b70647efbcbbab5cbf7f55e6ff336f843d628a1 |}];
    test
      ~scalar:
        (`Hex
          "0a0d622a47e48f6bc1038ace438c6f528aa00ad2bd1da5f13ee46bf5f633d71a")
      ~point:
        (`Hex
          "043cbc1b31b43f17dc200dd70c2944c04c6cb1b082820c234a300b05b7763844c74fde0a4ef93887469793270eb2ff148287da9265b0334f9e2609aac16e8ad503");
    [%expect
      {| 047fffffffffffffffffffffffeecf2230ffffffffffffffffffffffffffffffff00000001c7c30643abed0af0a49fe352cb483ff9b97dccdf427c658e8793240d |}];
    test
      ~scalar:
        (`Hex
          "55d55f11bb8da1ea318bca7266f0376662441ea87270aa2077f1b770c4854a48")
      ~point:
        (`Hex
          "04000000000000000000000000000000000000000000000000000000000000000066485c780e2f83d72433bd5d84a06bb6541c2af31dae871728bf856a174f93f4");
    [%expect
      {| 04cfe4077c8730b1c9384581d36bff5542bc417c9eff5c2afcb98cc8829b2ce8487764c65671a66a3ecf1ec63cf49b5c36119162ace73f8d8be270e27cdaf4677c |}]
end

module Dh = struct
  open Fiat_p256.For_tests

  let pp_result ppf = function
    | Ok cs -> Cstruct_util.pp_hex_le ppf cs
    | Error e -> Format.fprintf ppf "%a" Fiat_p256.pp_error e

  let from h _ = Hex.to_cstruct h

  let%expect_test "key_exchange" =
    let test d p =
      Format.printf "%a\n" pp_result (Fiat_p256.key_exchange d p)
    in
    let pub_a =
      `Hex "200102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
    in
    let pub_b =
      `Hex "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
    in
    let d_a, p_a = Fiat_p256.gen_key ~rng:(from pub_a) in
    let d_b, p_b = Fiat_p256.gen_key ~rng:(from pub_b) in
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
end
