type json

type hex = string [@@deriving eq]

val pp_hex : Format.formatter -> hex -> unit

type test_result =
  | Valid
  | Acceptable
  | Invalid
[@@deriving show]

type test =
  { tcId : int
  ; comment : string
  ; curve : json option
  ; public : hex option
  ; private_ : hex option
  ; shared : hex option
  ; result : test_result
  ; flags : string list
  ; msg : hex option
  ; sig_ : hex option }
[@@deriving show]

val has_ignored_flag : test -> ignored_flags:string list -> bool

type test_group =
  { curve : json option
  ; tests : test list
  ; encoding : json option
  ; type_ : json option
  ; key : json option
  ; keyDer : hex option
  ; keyPem : json option
  ; sha : json option }
[@@deriving show]

type test_file =
  { algorithm : json
  ; generatorVersion : json
  ; header : json
  ; notes : json
  ; numberOfTests : json
  ; testGroups : test_group list }
[@@deriving show]

val load_file_exn : string -> test_file
