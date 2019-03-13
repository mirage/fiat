type t =
  { f_x : Fe.t
  ; f_y : Fe.t
  ; f_z : Fe.t }

let at_infinity () =
  let f_x = Fe.one () in
  let f_y = Fe.one () in
  let f_z = Fe.create () in
  {f_x; f_y; f_z}

let of_cstruct cs =
  match (Cstruct.get_uint8 cs 0, Cstruct.len cs) with
  | 0x00, 1 ->
      Some (at_infinity ())
  | 0x04, 65 ->
      let f_x = Fe.create () in
      let f_y = Fe.create () in
      let buf_x = Cstruct.rev @@ Cstruct.sub cs 1 32 in
      let buf_y = Cstruct.rev @@ Cstruct.sub cs 33 32 in
      Fe.from_bytes f_x buf_x;
      Fe.to_montgomery f_x;
      Fe.from_bytes f_y buf_y;
      Fe.to_montgomery f_y;
      let f_z = Fe.one () in
      Some {f_x; f_y; f_z}
  | _ ->
      None

let of_hex h = of_cstruct (Hex.to_cstruct h)

let of_hex_exn h =
  match of_hex h with
  | Some p ->
      p
  | None ->
      failwith "of_hex_exn"

let to_affine p =
  let is_infty = not (Fe.nz p.f_z) in
  if is_infty then None
  else
    let out_x = Cstruct.create 32 in
    let out_y = Cstruct.create 32 in
    let z1 = Fe.create () in
    let z2 = Fe.create () in
    Fe.copy z1 p.f_z;
    Fe.inv z2 z1;
    Fe.sqr z1 z2;
    Fe.from_montgomery z1;
    let x = Fe.create () in
    Fe.copy x p.f_x;
    Fe.mul x x z1;
    Fe.to_bytes out_x x;
    let y = Fe.create () in
    Fe.copy y p.f_y;
    Fe.mul z1 z1 z2;
    Fe.mul y y z1;
    Fe.to_bytes out_y y;
    Some (out_x, out_y)

let to_cstruct p =
  match to_affine p with
  | None ->
      Cstruct.create 1
  | Some (x, y) ->
      let four = Cstruct.create 1 in
      Cstruct.set_uint8 four 0 4;
      let rev_x = Cstruct.rev x and rev_y = Cstruct.rev y in
      Cstruct.concat [four; rev_x; rev_y]

let pp fmt p = Cstruct_util.pp_hex_le fmt (Cstruct.rev (to_cstruct p))

external double_c : t -> t -> unit = "fiat_p256_caml_point_double" [@@noalloc]

let double p =
  let out = {f_x = Fe.create (); f_y = Fe.create (); f_z = Fe.create ()} in
  double_c out p; out

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

external add_c : t -> t -> t -> unit = "fiat_p256_caml_point_add" [@@noalloc]

let add fe_p fe_q =
  let out = {f_x = Fe.create (); f_y = Fe.create (); f_z = Fe.create ()} in
  add_c out fe_p fe_q; out

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

let x p =
  match to_affine p with
  | None ->
      Cstruct.create 1
  | Some (x, _) ->
      Cstruct.rev x
