open Pidgin

let%expect_test "construction: null" =
  let t = Data.Construct.null () in
  print_string (Render.to_string t);
  [%expect {| null |}]
;;

let%expect_test "construction: boolean true" =
  let t = Data.Construct.bool true in
  print_string (Render.to_string t);
  [%expect {| true |}]
;;

let%expect_test "construction: boolean false" =
  let t = Data.Construct.bool false in
  print_string (Render.to_string t);
  [%expect {| false |}]
;;

let%expect_test "construction: positive int" =
  let t = Data.Construct.int 42 in
  print_string (Render.to_string t);
  [%expect {| 42 |}]
;;

let%expect_test "construction: negative int" =
  let t = Data.Construct.int (-42) in
  print_string (Render.to_string t);
  [%expect {| -42 |}]
;;

let%expect_test "construction: positive float" =
  let t = Data.Construct.float 42.24 in
  print_string (Render.to_string t);
  [%expect {| 42.24 |}]
;;

let%expect_test "construction: negative float" =
  let t = Data.Construct.float (-42.24) in
  print_string (Render.to_string t);
  [%expect {| -42.24 |}]
;;

let%expect_test "construction: empty list" =
  let t = Data.Construct.list [] in
  print_string (Render.to_string t);
  [%expect {| [] |}]
;;

let%expect_test "construction: list with one element" =
  let t = Data.Construct.list [ Data.Construct.null () ] in
  print_string (Render.to_string t);
  [%expect {| [null] |}]
;;

let%expect_test "construction: list with two elements" =
  let t =
    Data.Construct.list [ Data.Construct.null (); Data.Construct.null () ]
  in
  print_string (Render.to_string t);
  [%expect {| [null,null] |}]
;;

let%expect_test "construction: list_of with two integers" =
  let t = Data.Construct.list_of Data.Construct.int [ 0; 1 ] in
  print_string (Render.to_string t);
  [%expect {| [0,1] |}]
;;

let%expect_test "construction: empty record" =
  let t = Data.Construct.record [] in
  print_string (Render.to_string t);
  [%expect {| {} |}]
;;

let%expect_test "construction: record with one element" =
  let t = Data.Construct.record [ "f", Data.Construct.null () ] in
  print_string (Render.to_string t);
  [%expect {| {"f":null} |}]
;;

let%expect_test "construction: record with two elements" =
  let t =
    Data.Construct.record
      [ "f1", Data.Construct.null (); "f2", Data.Construct.null () ]
  in
  print_string (Render.to_string t);
  [%expect {| {"f1":null,"f2":null} |}]
;;

let%expect_test "construction: record_of with two integers" =
  let t =
    Data.Construct.record_of
      (fun (k, v) -> k, Data.Construct.int v)
      [ "f1", 0; "f2", 1 ]
  in
  print_string (Render.to_string t);
  [%expect {| {"f1":0,"f2":1} |}]
;;
