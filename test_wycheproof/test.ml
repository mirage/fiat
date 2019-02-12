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

let rec strip_prefixes s ~prefixes =
  match prefixes with
  | [] ->
      None
  | prefix :: prefixes -> (
    match strip_prefix s ~prefix with
    | Some _ as r ->
        r
    | None ->
        strip_prefixes s ~prefixes )

let strip_asn1 =
  let prefixes =
    List.map Hex.to_string
      [`Hex "3059301306072a8648ce3d020106082a8648ce3d030107034200"]
  in
  strip_prefixes ~prefixes

let parse_point s =
  match strip_asn1 s with
  | Some payload ->
      Fiat_p256.point_of_hex (Hex.of_string payload)
  | None ->
      None

let parse_scalar s = Fiat_p256.scalar_of_hex (Hex.of_string s)

let test_valid ~private_ ~public ~expected () =
  match (parse_point public, parse_scalar private_) with
  | Some point, Some scalar ->
      let got = Cstruct.to_string (Fiat_p256.dh ~scalar ~point) in
      Alcotest.check hex "should be equal" expected got
  | _ ->
      failwith "cannot parse test case"

(*
let test_invalid ~private_ ~public () =
  Alcotest.check_raises "should raise" Unverified_api.Error (fun () ->
      let scalar = Unverified_api.parse_scalar private_ in
      let point = Unverified_api.parse_point public in
      ignore (Unverified_api.dh scalar point) )
   *)

let make_test {tcId; comment; private_; public; shared; result; flags; _} =
  let name = Printf.sprintf "%d - %s" tcId comment in
  let ignored_flags = [invalid_asn; compressed_point; unnamed_curve] in
  match result with
  | _
    when List.exists
           (fun ignored_flag -> List.mem ignored_flag flags)
           ignored_flags ->
      []
  | Valid
  | Acceptable ->
      [(name, `Quick, test_valid ~private_ ~public ~expected:shared)]
  | Invalid ->
      []

let tests =
  secp256r1.testGroups
  |> List.map (fun group -> List.map make_test group.tests |> List.concat)
  |> List.concat

let () = Alcotest.run "Wycheproof-hacl-p256" [("test vectors", tests)]
