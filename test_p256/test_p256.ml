let linkme = ()

module Cstruct_util = struct
  open Fiat_p256.For_tests.Cstruct_util

  let%expect_test "compare_be" =
    let test a b = print_int (compare_be a b) in
    test (Cstruct.of_string "aa") (Cstruct.of_string "ab");
    [%expect {| -1 |}];
    test (Cstruct.of_string "ab") (Cstruct.of_string "aa");
    [%expect {| 1 |}];
    test (Cstruct.of_string "aa") (Cstruct.of_string "aa");
    [%expect {| 0 |}];
    test (Cstruct.of_string "abx") (Cstruct.of_string "aaz");
    [%expect {| 1 |}];
    ()
end
