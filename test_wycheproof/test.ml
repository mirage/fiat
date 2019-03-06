open Wycheproof

let ( >>= ) xr f =
  match xr with
  | Error _ as e ->
      e
  | Ok x ->
      f x

let result_of_option ~msg = function
  | None ->
      Error msg
  | Some x ->
      Ok x

let get_exn = function
  | Some x ->
      x
  | None ->
      assert false

let concat_map f l = List.map f l |> List.concat

let hex = Alcotest.testable Wycheproof.pp_hex Wycheproof.equal_hex

let decode_ber term cs =
  match Asn.decode (Asn.codec Asn.ber term) cs with
  | Ok (x, rest)
    when Cstruct.len rest = 0 ->
      Some x
  | _ ->
      None
  | exception Invalid_argument _ ->
      (* https://github.com/mirleft/ocaml-asn1-combinators/issues/23 *)
      None

let parse_ber_key s =
  let cs = Cstruct.of_string s in
  let seq2 a b = Asn.S.(sequence2 (required a) (required b)) in
  let term = Asn.S.(seq2 (seq2 oid oid) bit_string_cs) in
  let ec_public_key = Asn.OID.(base 1 2 <|| [840; 10045; 2; 1]) in
  let prime256v1 = Asn.OID.(base 1 2 <|| [840; 10045; 3; 1; 7]) in
  match decode_ber term cs with
  | None ->
      Error "ASN1 parse error"
  | Some ((oid1, oid2), data) ->
      if not (Asn.OID.equal oid1 ec_public_key) then Error "ASN1: wrong oid 1"
      else if not (Asn.OID.equal oid2 prime256v1) then
        Error "ASN1: wrong oid 2"
      else Ok (Cstruct.to_string data)

let parse_public_key s =
  parse_ber_key s
  >>= fun payload ->
  result_of_option ~msg:"cannot parse point"
    (Fiat_p256.point_of_hex (Hex.of_string payload))

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
      Ok cs
  | n
    when n < 0 ->
      Error "input is too long"
  | pad_len ->
      Ok (Cstruct.append cs (Cstruct.create pad_len))

let test_name (tc : Wycheproof.test) =
  Printf.sprintf "%d - %s" tc.tcId tc.comment

let parse_ecdsa_signature cs =
  let asn_term =
    let open Asn.S in
    sequence2 (required integer) (required integer)
  in
  decode_ber asn_term cs

let parse_public_key_exn s =
  match parse_public_key (get_exn s) with
  | Error e ->
      failwith e
  | Ok x ->
      x

let parse_scalar s =
  let stripped = strip_leading_zeroes (Cstruct.of_string s) in
  pad ~total_len:32 (Cstruct.rev stripped)
  >>= fun cs -> Fiat_p256.scalar_of_cs (Cstruct.rev cs)

type test =
  { point : Fiat_p256.point
  ; scalar : Fiat_p256.scalar
  ; expected : string }

let interpret_test {point; scalar; expected} () =
  let got = Cstruct.to_string (Fiat_p256.dh ~scalar ~point) in
  Alcotest.check hex __LOC__ expected got

type invalid_test =
  { public : string
  ; private_ : string }

let is_ok = function
  | Ok _ ->
      true
  | Error _ ->
      false

let interpret_invalid_test {public; private_} () =
  let result =
    parse_public_key public
    >>= fun _ -> parse_scalar private_ >>= fun _ -> Ok ()
  in
  Alcotest.check Alcotest.bool __LOC__ false (is_ok result)

type ecdsa_test =
  { hash : Cstruct.t
  ; rs : (Cstruct.t * Cstruct.t) option
  ; key : Fiat_p256.point
  ; expected : bool }

type strategy =
  | Test of test
  | Invalid_test of invalid_test
  | ECDSA of ecdsa_test
  | Skip

let make_ecdh_test test =
  match test.result with
  | Acceptable ->
      Ok Skip
  | Invalid ->
      result_of_option test.public ~msg:"cannot get public"
      >>= fun public ->
      result_of_option test.private_ ~msg:"cannot get private"
      >>= fun private_ -> Ok (Invalid_test {public; private_})
  | Valid ->
      result_of_option test.public ~msg:"cannot get public"
      >>= fun public ->
      parse_public_key public
      >>= fun point ->
      result_of_option test.private_ ~msg:"cannot get private"
      >>= fun private_ ->
      parse_scalar private_
      >>= fun scalar ->
      result_of_option test.shared ~msg:"cannot get shared"
      >>= fun expected -> Ok (Test {point; scalar; expected})

let interpret_ecdsa_test {hash; rs; key; expected} () =
  let got =
    match rs with
    | None ->
        false
    | Some (r, s) ->
        Fiat_p256.verify hash key ~r ~s
  in
  Alcotest.check Alcotest.bool __LOC__ expected got

let interpret x =
  let name = test_name x in
  function
  | Ok (Test t) ->
      [(name, `Quick, interpret_test t)]
  | Ok (Invalid_test t) ->
      [(name, `Quick, interpret_invalid_test t)]
  | Ok (ECDSA t) ->
      [(name, `Quick, interpret_ecdsa_test t)]
  | Ok Skip ->
      []
  | Error e ->
      Printf.ksprintf failwith "While parsing %d: %s" x.tcId e

let interpret_json_file path ~ignored_flags ?(ignored_ids = []) k =
  let data = load_file_exn path in
  concat_map
    (fun group ->
      concat_map
        (fun x ->
          let r =
            if
              Wycheproof.has_ignored_flag x ~ignored_flags
              || List.mem x.tcId ignored_ids
            then Ok Skip
            else k group x
          in
          interpret x r )
        group.tests )
    data.testGroups

let parse_signature cs =
  let cs_of_z z = Z.to_bits z |> Cstruct.of_string |> Cstruct.rev in
  match parse_ecdsa_signature cs with
  | Some (zr, zs) ->
      Some (cs_of_z zr, cs_of_z zs)
  | None ->
      None

let make_ecdsa_test algo key (tc : Wycheproof.test) =
  let truncate cs = Cstruct.sub cs 0 32 in
  let hash =
    get_exn tc.msg
    |> Digestif.digest_string algo
    |> Digestif.to_raw_string algo
    |> Cstruct.of_string
    |> truncate
    (* XXX should this be done in verify? *)
  in
  let rs = get_exn tc.sig_ |> Cstruct.of_string |> parse_signature in
  match tc.result with
  | Valid ->
      Ok (ECDSA {hash; rs; key; expected = true})
  | Invalid ->
      Ok (ECDSA {hash; rs; key; expected = false})
  | Acceptable ->
      Ok Skip

let () =
  let asn1_int64_overflow =
    (* https://github.com/mirleft/ocaml-asn1-combinators/issues/24 *)
    [19; 20; 21]
  in
  Alcotest.run "Wycheproof-hacl-p256"
    [ ( "ECDH"
      , interpret_json_file "ecdh_secp256r1_test.json"
          ~ignored_flags:["CompressedPoint"; "UnnamedCurve"] (fun _ x ->
            make_ecdh_test x ) )
    ; ( "ECDSA+SHA256"
      , interpret_json_file "ecdsa_secp256r1_sha256_test.json"
          ~ignored_flags:["EdgeCase"; "BER"; "PointDuplication"]
          ~ignored_ids:(asn1_int64_overflow @ [133; 139; 140])
          (fun group ->
            make_ecdsa_test Digestif.SHA256
              (parse_public_key_exn group.keyDer) ) )
    ; ( "ECDSA+SHA512"
      , interpret_json_file "ecdsa_secp256r1_sha512_test.json"
          ~ignored_flags:["BER"] ~ignored_ids:asn1_int64_overflow
          (fun group ->
            make_ecdsa_test Digestif.SHA512
              (parse_public_key_exn group.keyDer) ) ) ]
