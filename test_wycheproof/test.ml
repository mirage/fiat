open Wycheproof

let hex = Alcotest.testable Wycheproof.pp_hex Wycheproof.equal_hex

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

let point_error_to_string = function
  | `CoordinateTooLarge -> "coordinate out of range"
  | `InvalidFormat -> "invalid format"
  | `InvalidLength -> "invalid length"
  | `NotOnCurve -> "point is not on curve"

let scalar_error_to_string = function
  | `InvalidLength -> "input has incorrect length"
  | `InvalidRange -> "input is not in [1; n-1]"

let to_string_result ~prefix ~err_to_str = function
  | Ok _ as ok -> ok
  | Error e -> 
    let msg = Printf.sprintf "%s : %s" 
      prefix 
      (err_to_str e) 
    in Error msg

let parse_point s =
  parse_asn1 s
  >>= fun payload ->
  to_string_result 
    ~prefix:"cannot parse point" 
    ~err_to_str:point_error_to_string
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

let parse_scalar s =
  let stripped = strip_leading_zeroes (Cstruct.of_string s) in
  pad ~total_len:32 (Cstruct.rev stripped)
  >>= fun cs -> 
    to_string_result
    ~prefix:"cannot parse scalar"
    ~err_to_str:scalar_error_to_string
    (Fiat_p256.scalar_of_cs (Cstruct.rev cs))

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
