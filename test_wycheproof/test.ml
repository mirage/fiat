open Wycheproof

let hex = Alcotest.testable Wycheproof.pp_hex Wycheproof.equal_hex

let strip_prefix s ~prefix =
  let prefix_len = String.length prefix in
  let s_len = String.length s in
  if prefix_len > s_len then None
  else
    let prefix_in_s = String.sub s 0 prefix_len in
    if String.equal prefix prefix_in_s then
      Some (Str.string_after s prefix_len)
    else None

let result_of_option ~msg = function
  | None ->
      Error msg
  | Some x ->
      Ok x

let strip_asn1 s =
  let prefix =
    Hex.to_string
      (`Hex "3059301306072a8648ce3d020106082a8648ce3d030107034200")
  in
  result_of_option ~msg:"unknown ASN1 prefix" (strip_prefix ~prefix s)

let ( >>= ) xr f =
  match xr with
  | Error _ as e ->
      e
  | Ok x ->
      f x

let parse_point s =
  strip_asn1 s
  >>= fun payload ->
  result_of_option ~msg:"cannot parse point"
    (Fiat_p256.point_of_hex (Hex.of_string payload))

let parse_scalar s =
  result_of_option ~msg:"cannot parse scalar"
    (Fiat_p256.scalar_of_hex (Hex.of_string s))

type test =
  { name : string
  ; point : Fiat_p256.point
  ; scalar : Fiat_p256.scalar
  ; expected : string }

let interpret_test {name; point; scalar; expected} =
  let run () =
    let got = Cstruct.to_string (Fiat_p256.dh ~scalar ~point) in
    Alcotest.check hex __LOC__ expected got
  in
  (name, `Quick, run)

type strategy =
  | Test of test
  | Skip

let test_name test = Printf.sprintf "%d - %s" test.tcId test.comment

let make_test test =
  let ignored_flags = ["InvalidAsn"; "CompressedPoint"; "UnnamedCurve"] in
  match test.result with
  | _
    when has_ignored_flag test ~ignored_flags ->
      Ok Skip
  | Invalid ->
      Ok Skip
  | Valid
   |Acceptable ->
      parse_point test.public
      >>= fun point ->
      parse_scalar test.private_
      >>= fun scalar ->
      let name = test_name test in
      Ok (Test {name; point; scalar; expected = test.shared})

let concat_map f l = List.map f l |> List.concat

let to_tests x =
  match make_test x with
  | Ok (Test t) ->
      [interpret_test t]
  | Ok Skip ->
      []
  | Error e ->
      failwith e

let tests =
  let data = load_file_exn "ecdh_secp256r1_test.json" in
  concat_map (fun group -> concat_map to_tests group.tests) data.testGroups

let () = Alcotest.run "Wycheproof-hacl-p256" [("test vectors", tests)]
