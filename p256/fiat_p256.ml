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

let cstruct_of_fe fe =
  let cs = Cstruct.create 32 in
  let fe_not_mtg = Fe.create () in
  Fe.copy fe_not_mtg fe;
  Fe.from_montgomery fe_not_mtg;
  Fe.to_bytes cs fe_not_mtg;
  Cstruct.rev cs

let scalar_of_fe fe =
  let cs = cstruct_of_fe fe in
  match Scalar.of_cstruct cs with
  | Ok x ->
      x
  | Error _ ->
      assert false

let fe_from_hex h = Hex.to_cstruct h |> Fe.from_be_cstruct

let%expect_test "cstruct_of_fe" =
  let fe =
    fe_from_hex
      (`Hex
        "0000000000000000000000000000000000000000000000000000000000000001")
  in
  let cs = cstruct_of_fe fe in
  Format.printf "%a" Cstruct_util.pp_hex_le cs;
  [%expect
    {| 0100000000000000000000000000000000000000000000000000000000000000 |}]

let%expect_test "scalar_of_fe" =
  let fe =
    fe_from_hex
      (`Hex
        "0000000000000000000000000000000000000000000000000000000000000001")
  in
  let scalar = scalar_of_fe fe in
  Printf.printf "%b" (Scalar.bit_at scalar 0);
  [%expect {| true |}];
  let r = ref false in
  for i = 1 to 255 do
    r := !r || Scalar.bit_at scalar i
  done;
  Printf.printf "%b" !r;
  [%expect {| false |}]

let z_of_hex (`Hex s) =
  let z = Z.of_string_base 16 s in
  z

let%expect_test "z_of_hex" =
  let test h = Z.print (z_of_hex h) in
  test (`Hex "f000000000000000000000000000000000000000000000000000000000000000");
  [%expect{| 108555083659983933209597798445644913612440610624038028786991485007418559037440 |}]

let pp_z_hex fmt z =
  let bits = Z.to_bits z in
  for i = String.length bits - 1 downto 0 do
    Format.fprintf fmt "%02x" (Char.code bits.[i])
  done

let scalar_of_z z =
  let s = Format.asprintf "%a" pp_z_hex z in
  Scalar.of_hex_exn (`Hex s)

type verify_steps =
  { w : Z.t
  ; u : Z.t
  ; v : Z.t
  ; gu : point
  ; gwv : point
  ; sum : point
  ; x_z : Z.t
  ; ok : bool }

let pp_steps fmt {w; u; v; gu; gwv; sum; x_z; ok} =
  Format.fprintf fmt
    "w:   %a\nu:   %a\nv:   %a\ngu:  %a\ngwv: %a\nsum: %a\nx_z: %a\nok:  %b"
    pp_z_hex w pp_z_hex u pp_z_hex v Point.pp gu Point.pp gwv Point.pp sum
    pp_z_hex x_z ok

let verify_steps ~r ~s ~h ~pub_key =
  let n_hex =
    `Hex "FFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632551"
  in
  let n = z_of_hex n_hex in
  let in_range x = Z.compare x Z.one >= 0 && Z.compare n x > 0 in
  let mul_mod a b =
    let open Z in
    a * b mod n
  in
  let equal_mod a b =
    let open Z in
    equal (a mod n) (b mod n)
  in
  if in_range r && in_range s then
    let w = Z.invert s n in
    let u = mul_mod h w in
    let v = mul_mod r w in
    let gu = Montgomery_ladder.scalar_mult (scalar_of_z u) base_point in
    let gwv = Montgomery_ladder.scalar_mult (scalar_of_z v) pub_key in
    let sum = Point.add gu gwv in
    let x = Point.x_of_finite_point sum in
    let x_z = Z.of_bits (Cstruct.to_string (Cstruct.rev x)) in
    let ok = equal_mod x_z r in
    Some {w; u; v; gu; gwv; sum; x_z; ok}
  else None

let verify msg pub_key ~r ~s =
  let convert x = z_of_hex (Hex.of_cstruct x) in
  let h = convert msg in
  let r = convert r in
  let s = convert s in
  match verify_steps ~r ~s ~h ~pub_key with
  | Some steps ->
      steps.ok
  | None ->
      false

let pp_opt pp fmt = function
  | None ->
      Format.fprintf fmt "None"
  | Some x ->
      pp fmt x

let%expect_test "verify_steps" =
  (* RFC 4754 § 8.1 *)
  let priv_key =
    Scalar.of_hex_exn
      (`Hex
        "DC51D3866A15BACDE33D96F992FCA99DA7E6EF0934E7097559C27F1614C88A7F")
  in
  let pub_key = public priv_key in
  let h_hex =
    `Hex "BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD"
  in
  let h = z_of_hex h_hex in
  let test ~r ~s =
    let r = z_of_hex r in
    let s = z_of_hex s in
    let steps = verify_steps ~r ~s ~h ~pub_key in
    Format.printf "%a" (pp_opt pp_steps) steps
  in
  test
    ~r:
      (`Hex
        "CB28E0999B9C7715FD0A80D8E47A77079716CBBF917DD72E97566EA1C066957C")
    ~s:
      (`Hex
        "86FA3BB4E26CAD5BF90B7F81899256CE7594BB1EA0C89212748BFF3B3D5B0315");
  [%expect
    {|
    w:   33bdc294e90cfad62a9f2fd1f8741da77c02a573e1b53ba17a60ba904f491952
    u:   c3875e57c85038a0d60370a87505200dc8317c8c534948bea6559c7c18e6d4ce
    v:   3b4e49c4fdbfc006ff993c81a50eae221149076d6ec09ddd9fb3b787f85b6483
    gu:  044f7497629362efbbee591206d036568f239789b234960635c6607ec6990626008490e12de4dbb68cbf9417215d8c648e57a8e0e44e1768563cd58697001a8d08
    gwv: 04726e5684964db8ea341d8679dfb70e04eda404e994ba730fa43f1e78ed81211b0c10cba8dd2620c112a4f9be578e4be1e64dc0f7d1d526ca167749f9cec0df08
    sum: 04cb28e0999b9c7715fd0a80d8e47a77079716cbbf917dd72e97566ea1c066957c2b57c0235fb7489768d058ff4911c20fdbe71e3699d91339afbb903ee17255dc
    x_z: cb28e0999b9c7715fd0a80d8e47a77079716cbbf917dd72e97566ea1c066957c 
    ok:  true
|}];
  test
    ~r:
      (`Hex
        "555555550000000055555555555555553ef7a8e48d07df81a693439654210c70")
    ~s:(`Hex "00");
  [%expect {| None |}]
