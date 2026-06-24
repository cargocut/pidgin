open Pidgin

let%expect_test "construct: null" =
  let t = Kind.Construct.nothing () in
  print_string (Kind_render.to_string t);
  [%expect {| nothing |}]
;;

let%expect_test "construct: opt int" =
  let t = Kind.Construct.opt (Kind.Construct.int ()) in
  print_string (Kind_render.to_string t);
  [%expect {| int? |}]
;;

let%expect_test "construct: boolean" =
  let t = Kind.Construct.bool () in
  print_string (Kind_render.to_string t);
  [%expect {| bool |}]
;;

let%expect_test "construct: int" =
  let t = Kind.Construct.int () in
  print_string (Kind_render.to_string t);
  [%expect {| int |}]
;;

let%expect_test "construct: float" =
  let t = Kind.Construct.float () in
  print_string (Kind_render.to_string t);
  [%expect {| float |}]
;;

let%expect_test "construct: string" =
  let t = Kind.Construct.string () in
  print_string (Kind_render.to_string t);
  [%expect {| string |}]
;;

let%expect_test "construct: empty list" =
  let t = Kind.Construct.list [] in
  print_string (Kind_render.to_string t);
  [%expect {| [] |}]
;;

let%expect_test "construct: list with one element" =
  let t = Kind.Construct.list [ Kind.Construct.nothing () ] in
  print_string (Kind_render.to_string t);
  [%expect {| [nothing] |}]
;;

let%expect_test "construct: list with two elements" =
  let t =
    Kind.Construct.list [ Kind.Construct.nothing (); Kind.Construct.nothing () ]
  in
  print_string (Kind_render.to_string t);
  [%expect {| [nothing,nothing] |}]
;;

let%expect_test "construct: list_of with two integers" =
  let t = Kind.Construct.list_of Kind.Construct.int [ 0; 1 ] in
  print_string (Kind_render.to_string t);
  [%expect {| [int,int] |}]
;;

let%expect_test "construct: empty record" =
  let t = Kind.Construct.record [] in
  print_string (Kind_render.to_string t);
  [%expect {| {} |}]
;;

let%expect_test "construct: record with one element" =
  let t = Kind.Construct.record [ "f", Kind.Construct.nothing () ] in
  print_string (Kind_render.to_string t);
  [%expect {| {"f":nothing} |}]
;;

let%expect_test "construct: record with two elements" =
  let t =
    Kind.Construct.record
      [ "f1", Kind.Construct.nothing (); "f2", Kind.Construct.nothing () ]
  in
  print_string (Kind_render.to_string t);
  [%expect {| {"f1":nothing,"f2":nothing} |}]
;;

let%expect_test "construct: record_of with two integers" =
  let t =
    Kind.Construct.record_of
      (fun (k, v) -> k, Kind.Construct.int v)
      [ "f1", 0; "f2", 1 ]
  in
  print_string (Kind_render.to_string t);
  [%expect {| {"f1":int,"f2":int} |}]
;;

let%expect_test "construct: record_of with heterogeneous values" =
  let t =
    Kind.Construct.record
      [ "f1", Kind.Construct.int 0; "f2", Kind.Construct.string "1" ]
  in
  print_string (Kind_render.to_string t);
  [%expect {| {"f1":int,"f2":string} |}]
;;
