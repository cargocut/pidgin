open Pidgin

let string_of_result = function
  | Ok t -> "Ok"
  | Error (Validation.Invalid_shape { expected; given }) ->
    "Error: " ^ Render.to_string given ^ " is not a " ^ expected
  | Error (Validation.Invalid_list { errors; given }) -> "Error: TODO "
;;

let%expect_test "validation: null" =
  let t = Data.Construct.null in
  print_string (string_of_result (Validation.null t));
  [%expect {| Ok |}]
;;

let%expect_test "validation: not null" =
  let t = Data.Construct.bool true in
  print_string (string_of_result (Validation.null t));
  [%expect {| Error: true is not a null |}]
;;

let%expect_test "validation: bool" =
  let t = Data.Construct.bool true in
  print_string (string_of_result (Validation.bool t));
  [%expect {| Ok |}]
;;

let%expect_test "validation: not bool" =
  let t = Data.Construct.null in
  print_string (string_of_result (Validation.bool t));
  [%expect {| Error: null is not a bool |}]
;;

let%expect_test "validation: int" =
  let t = Data.Construct.int 42 in
  print_string (string_of_result (Validation.int t));
  [%expect {| Ok |}]
;;

let%expect_test "validation: not int" =
  let t = Data.Construct.null in
  print_string (string_of_result (Validation.int t));
  [%expect {| Error: null is not a int |}]
;;

let%expect_test "validation: float" =
  let t = Data.Construct.float 42.42 in
  print_string (string_of_result (Validation.float t));
  [%expect {| Ok |}]
;;

let%expect_test "validation: not float" =
  let t = Data.Construct.null in
  print_string (string_of_result (Validation.float t));
  [%expect {| Error: null is not a float |}]
;;

let%expect_test "validation: string" =
  let t = Data.Construct.string "hello" in
  print_string (string_of_result (Validation.string t));
  [%expect {| Ok |}]
;;

let%expect_test "validation: not string" =
  let t = Data.Construct.null in
  print_string (string_of_result (Validation.string t));
  [%expect {| Error: null is not a string |}]
;;

let%expect_test "validation: un restricted string with bool" =
  let t = Data.Construct.bool true in
  print_string (string_of_result (Validation.string ~strict:false t));
  [%expect {| Ok |}]
;;

let%expect_test "validation: un restricted string with int" =
  let t = Data.Construct.int 42 in
  print_string (string_of_result (Validation.string ~strict:false t));
  [%expect {| Ok |}]
;;

let%expect_test "validation: un restricted string with float" =
  let t = Data.Construct.float 42.42 in
  print_string (string_of_result (Validation.string ~strict:false t));
  [%expect {| Ok |}]
;;
