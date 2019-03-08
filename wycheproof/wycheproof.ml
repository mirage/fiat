[@@@ocaml.warning "-39"]

type json = Yojson.Safe.json [@@deriving of_yojson]

let pp_json = Yojson.Safe.pretty_print

type hex = string [@@deriving eq]

let pp_hex fmt s =
  let (`Hex h) = Hex.of_string s in
  Format.pp_print_string fmt h

let hex_of_yojson json =
  let padded s = if String.length s mod 2 = 0 then s else "0" ^ s in
  match [%of_yojson: string] json with
  | Ok s ->
      Ok (Hex.to_string (`Hex (padded s)))
  | Error _ as e ->
      e

type test_result =
  | Valid
  | Acceptable
  | Invalid
[@@deriving show]

let test_result_of_yojson = function
  | `String "valid" ->
      Ok Valid
  | `String "acceptable" ->
      Ok Acceptable
  | `String "invalid" ->
      Ok Invalid
  | _ ->
      Error "test_result"

type flag =
  | Twist
  | LowOrderPublic
  | SmallPublicKey
  | CompressedPoint
  | AddSubChain
  | InvalidPublic
  | WrongOrder
  | UnnamedCurve
  | UnusedParam
  | ModifiedPrime
  | WeakPublicKey
  | InvalidAsn
[@@deriving show]

let invalid_asn = InvalidAsn

let compressed_point = CompressedPoint

let unnamed_curve = UnnamedCurve

let flag_of_yojson = function
  | `String "LowOrderPublic" ->
      Ok LowOrderPublic
  | `String "Twist" ->
      Ok Twist
  | `String "Small public key" ->
      Ok SmallPublicKey
  | `String "CompressedPoint" ->
      Ok CompressedPoint
  | `String "AddSubChain" ->
      Ok AddSubChain
  | `String "InvalidPublic" ->
      Ok InvalidPublic
  | `String "WrongOrder" ->
      Ok WrongOrder
  | `String "UnnamedCurve" ->
      Ok UnnamedCurve
  | `String "UnusedParam" ->
      Ok UnusedParam
  | `String "ModifiedPrime" ->
      Ok ModifiedPrime
  | `String "WeakPublicKey" ->
      Ok WeakPublicKey
  | `String "InvalidAsn" ->
      Ok InvalidAsn
  | `String s ->
      Error ("Unknown flag: " ^ s)
  | _ ->
      Error "flag_of_yojson"

type test =
  { tcId : int
  ; comment : string
  ; curve : json option [@yojson.default None]
  ; public : hex
  ; private_ : hex [@yojson.key "private"]
  ; shared : hex
  ; result : test_result
  ; flags : flag list }
[@@deriving of_yojson, show]

type test_group =
  { curve : json
  ; tests : test list
  ; encoding : json option [@yojson.default None]
  ; type_ : json option [@yojson.default None] [@yojson.key "type"] }
[@@deriving of_yojson, show]

type test_file =
  { algorithm : json
  ; generatorVersion : json
  ; header : json
  ; notes : json
  ; numberOfTests : json
  ; testGroups : test_group list }
[@@deriving of_yojson, show]

let get_json = function
  | Ok x ->
      x
  | Error s ->
      failwith s

let load_tests s =
  Yojson.Safe.from_string s |> [%of_yojson: test_file] |> get_json

let secp256r1 = load_tests [%blob "ecdh_secp256r1_test.json"]
