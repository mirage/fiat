open Wycheproof

let hex = Alcotest.testable Wycheproof.pp_hex Wycheproof.equal_hex

let result_of_option ~msg = function
  | None ->
      Error msg
  | Some x ->
      Ok x

let parse_asn1 s =
  let cs = Cstruct.of_string s in
  let seq2 a b = Asn.S.(sequence2 (required a) (required b)) in
  let term = Asn.S.(seq2 (seq2 oid oid) bit_string_cs) in
  let ec_public_key = Asn.OID.(base 1 2 <|| [840; 10045; 2; 1]) in
  let prime256v1 = Asn.OID.(base 1 2 <|| [840; 10045; 3; 1; 7]) in
  match Asn.decode (Asn.codec Asn.ber term) cs with
  | Error _ ->
      Error "ASN1 parse error"
  | Ok (((oid1, oid2), data), rest) ->
      if Cstruct.len rest <> 0 then Error "ASN1 leftover"
      else if not (Asn.OID.equal oid1 ec_public_key) then
        Error "ASN1: wrong oid 1"
      else if not (Asn.OID.equal oid2 prime256v1) then
        Error "ASN1: wrong oid 2"
      else Ok (Cstruct.to_string data)

let ( >>= ) xr f =
  match xr with
  | Error _ as e ->
      e
  | Ok x ->
      f x

let parse_point s =
  parse_asn1 s
  >>= fun payload ->
  result_of_option ~msg:"cannot parse point"
    (Fiat_p256.point_of_hex (Hex.of_string payload))

let parse_scalar s =
  result_of_option ~msg:"cannot parse scalar"
    (Fiat_p256.scalar_of_hex (Hex.of_string s))

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
    parse_point public >>= fun _ -> parse_scalar private_ >>= fun _ -> Ok ()
  in
  Alcotest.check Alcotest.bool __LOC__ false (is_ok result)

type strategy =
  | Test of test
  | Invalid_test of invalid_test
  | Skip

let make_test test =
  let ignored_flags = ["CompressedPoint"; "UnnamedCurve"] in
  match test.result with
  | _
    when has_ignored_flag test ~ignored_flags ->
      Ok Skip
  | _
    when test.comment = "point is not on curve"
         || List.mem test.tcId [92; 93; 94] ->
      (* Disable tests with invalid points - see #3 *)
      Ok Skip
  | Invalid ->
      Ok (Invalid_test {public = test.public; private_ = test.private_})
  | Acceptable ->
      Ok Skip
  | Valid ->
      parse_point test.public
      >>= fun point ->
      parse_scalar test.private_
      >>= fun scalar -> Ok (Test {point; scalar; expected = test.shared})

let concat_map f l = List.map f l |> List.concat

let to_tests x =
  let name = Printf.sprintf "%d - %s" x.tcId x.comment in
  match make_test x with
  | Ok (Test t) ->
      [(name, `Quick, interpret_test t)]
  | Ok (Invalid_test t) ->
      [(name, `Quick, interpret_invalid_test t)]
  | Ok Skip ->
      []
  | Error e ->
      Printf.ksprintf failwith "While parsing %d: %s" x.tcId e

let tests =
  let data = load_file_exn "ecdh_secp256r1_test.json" in
  concat_map (fun group -> concat_map to_tests group.tests) data.testGroups

let () = Alcotest.run "Wycheproof-hacl-p256" [("test vectors", tests)]
