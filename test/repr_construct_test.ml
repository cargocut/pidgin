open Pidgin

let%expect_test "construct: null" =
  let t = Repr.Construct.null () in
  print_string (Render_repr.to_string t);
  [%expect {| null |}]
;;

let%expect_test "construct: boolean true" =
  let t = Repr.Construct.bool true in
  print_string (Render_repr.to_string t);
  [%expect {| true |}]
;;

let%expect_test "construct: boolean false" =
  let t = Repr.Construct.bool false in
  print_string (Render_repr.to_string t);
  [%expect {| false |}]
;;

let%expect_test "construct: positive int" =
  let t = Repr.Construct.int 42 in
  print_string (Render_repr.to_string t);
  [%expect {| 42 |}]
;;

let%expect_test "construct: negative int" =
  let t = Repr.Construct.int (-42) in
  print_string (Render_repr.to_string t);
  [%expect {| -42 |}]
;;

let%expect_test "construct: positive float" =
  let t = Repr.Construct.float 42.24 in
  print_string (Render_repr.to_string t);
  [%expect {| 42.24 |}]
;;

let%expect_test "construct: empty list" =
  let t = Repr.Construct.list [] in
  print_string (Render_repr.to_string t);
  [%expect {| [] |}]
;;

let%expect_test "construct: empty string" =
  let t = Repr.Construct.string "" in
  print_string (Render_repr.to_string t);
  [%expect {| "" |}]
;;

let%expect_test "construct: not empty string" =
  let t = Repr.Construct.string "42" in
  print_string (Render_repr.to_string t);
  [%expect {| "42" |}]
;;

let%expect_test "construct: list with one element" =
  let t = Repr.Construct.list [ Repr.Construct.null () ] in
  print_string (Render_repr.to_string t);
  [%expect {| [null] |}]
;;

let%expect_test "construct: list with two elements" =
  let t =
    Repr.Construct.list [ Repr.Construct.null (); Repr.Construct.null () ]
  in
  print_string (Render_repr.to_string t);
  [%expect {| [null,null] |}]
;;

let%expect_test "construct: list_of with two integers" =
  let t = Repr.Construct.list_of Repr.Construct.int [ 0; 1 ] in
  print_string (Render_repr.to_string t);
  [%expect {| [0,1] |}]
;;

let%expect_test "construct: empty record" =
  let t = Repr.Construct.record [] in
  print_string (Render_repr.to_string t);
  [%expect {| {} |}]
;;

let%expect_test "construct: record with one element" =
  let t = Repr.Construct.record [ "f", Repr.Construct.null () ] in
  print_string (Render_repr.to_string t);
  [%expect {| {"f":null} |}]
;;

let%expect_test "construct: record with two elements" =
  let t =
    Repr.Construct.record
      [ "f1", Repr.Construct.null (); "f2", Repr.Construct.null () ]
  in
  print_string (Render_repr.to_string t);
  [%expect {| {"f1":null,"f2":null} |}]
;;

let%expect_test "construct: record_of with two integers" =
  let t =
    Repr.Construct.record_of
      (fun (k, v) -> k, Repr.Construct.int v)
      [ "f1", 0; "f2", 1 ]
  in
  print_string (Render_repr.to_string t);
  [%expect {| {"f1":0,"f2":1} |}]
;;

let%expect_test "construct: record_of with heterogeneous values" =
  let t =
    Repr.Construct.record
      [ "f1", Repr.Construct.int 0; "f2", Repr.Construct.string "1" ]
  in
  print_string (Render_repr.to_string t);
  [%expect {| {"f1":0,"f2":"1"} |}]
;;
