type json

type hex = string [@@deriving eq]

val pp_hex : Format.formatter -> hex -> unit

type test_result =
  | Valid
  | Acceptable
  | Invalid
[@@deriving show]

type flag [@@deriving show]

val invalid_asn : flag

val compressed_point : flag

val unnamed_curve : flag

type test =
  { tcId : int
  ; comment : string
  ; curve : json option
  ; public : hex
  ; private_ : hex
  ; shared : hex
  ; result : test_result
  ; flags : flag list }
[@@deriving show]

type test_group =
  { curve : json
  ; tests : test list
  ; encoding : json option
  ; type_ : json option }
[@@deriving show]

type test_file =
  { algorithm : json
  ; generatorVersion : json
  ; header : json
  ; notes : json
  ; numberOfTests : json
  ; testGroups : test_group list }
[@@deriving show]

val secp256r1 : test_file
