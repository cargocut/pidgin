(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

let dump ?(fmt = fun ppf _ -> Format.fprintf ppf "checked") v x =
  x |> v |> Format.asprintf "%a" (Pp.checked_value fmt) |> print_endline
;;

let%expect_test "validate null" =
  let check = Check.null
  and repr = Repr.Null in
  dump check repr;
  [%expect {| Ok checked |}]
;;

let%expect_test "validate null" =
  let check = Check.null
  and repr = Repr.int 123 in
  dump check repr;
  [%expect
    {|
    Error {"kind": "unexpected_kind", "value": 123, "expected": "null",
           "given": "int"}
    |}]
;;

let%expect_test "validate bool" =
  let check = Check.bool
  and repr = Repr.bool true in
  dump check repr;
  [%expect {| Ok checked |}]
;;

let%expect_test "validate bool" =
  let check = Check.bool
  and repr = Repr.bool false in
  dump check repr;
  [%expect {| Ok checked |}]
;;

let%expect_test "validate bool" =
  let check = Check.bool
  and repr = Repr.(list [ bool false; int 32; string "foo" ]) in
  dump check repr;
  [%expect
    {|
    Error {"kind": "unexpected_kind", "value": [false, 32, "foo"],
           "expected": "bool", "given": "[(bool | int | string)]"}
    |}]
;;

let%expect_test "validate int" =
  let check = Check.int
  and repr = Repr.int 1234 in
  dump check repr;
  [%expect {| Ok checked |}]
;;

let%expect_test "validate int" =
  let check = Check.int
  and repr = Repr.int (-1234) in
  dump check repr;
  [%expect {| Ok checked |}]
;;

let%expect_test "validate int" =
  let check = Check.int
  and repr = Repr.(pair int (result ~ok:string ~error:null) (1, Ok "hello")) in
  dump check repr;
  [%expect
    {|
    Error {"kind": "unexpected_kind",
           "value": {"first": 1, "second": {"constr": "ok", "value": "hello"}},
           "expected": "int", "given": "(int * #ok<string>)"}
    |}]
;;

let%expect_test "validate float" =
  let check = Check.float
  and repr = Repr.float 1234.89 in
  dump check repr;
  [%expect {| Ok checked |}]
;;

let%expect_test "validate float" =
  let check = Check.float
  and repr = Repr.int (-1234) in
  dump check repr;
  [%expect {| Ok checked |}]
;;

let%expect_test "validate float" =
  let check = Check.int
  and repr =
    Repr.(
      pair
        record
        (result ~ok:string ~error:null)
        ([ "foo", either ~left:int ~right:string (Left 43) ], Ok "hello"))
  in
  dump check repr;
  [%expect
    {|
    Error {"kind": "unexpected_kind",
           "value":
            {"first": {"foo": {"constr": "left", "value": 43}},
             "second": {"constr": "ok", "value": "hello"}}, "expected": "int",
           "given": "({\"foo\": #left<int>} * #ok<string>)"}
    |}]
;;

let%expect_test "validate string" =
  let check = Check.string
  and repr = Repr.string "Hello World" in
  dump check repr;
  [%expect {| Ok checked |}]
;;

let%expect_test "validate string" =
  let check = Check.string
  and repr = Repr.(list_of string) [ "Hello World" ] in
  dump check repr;
  [%expect
    {|
    Error {"kind": "unexpected_kind", "value": ["Hello World"],
           "expected": "string", "given": "[string]"}
    |}]
;;

let%expect_test "validate list" =
  let check = Check.(list_of string)
  and repr = Repr.(list_of string) [ "Hello World" ] in
  dump check repr;
  [%expect {| Ok checked |}]
;;

let%expect_test "validate list" =
  let check = Check.(list_of (list_of int))
  and repr = Repr.(list_of (list_of int)) [ [ 1; 3; 4 ]; []; [ 3012 ] ] in
  dump check repr;
  [%expect {| Ok checked |}]
;;

let%expect_test "validate list" =
  let check = Check.(list_of (list_of int))
  and repr = Repr.(list_of string) [ "Hello" ] in
  dump check repr;
  [%expect
    {|
    Error {"kind": "invalid_list", "value": ["Hello"],
           "errors":
            [{"at": 0,
              "error":
               {"kind": "unexpected_kind", "value": "Hello",
                "expected": "[?any]", "given": "string"}}]}
    |}]
;;

let%expect_test "validate list" =
  let check = Check.(list_of (list_of int))
  and repr = Repr.(list [ list []; list [ int 1; int 3; bool true ] ]) in
  dump check repr;
  [%expect
    {|
    Error {"kind": "invalid_list", "value": [[], [1, 3, true]],
           "errors":
            [{"at": 1,
              "error":
               {"kind": "invalid_list", "value": [1, 3, true],
                "errors":
                 [{"at": 2,
                   "error":
                    {"kind": "unexpected_kind", "value": true, "expected": "int",
                     "given": "bool"}}]}}]}
    |}]
;;

let%expect_test "validate record" =
  let check =
    let open Check in
    record (fun fields ->
      let+ name = req fields "name" string
      and+ nickname = opt ~alt:[ "pseudo" ] fields "nickname" string
      and+ age = opt fields "age" int in
      name, nickname, age)
  and repr = Repr.(record [ "name", string "Mick" ]) in
  dump check repr;
  [%expect {| Ok checked |}]
;;

let%expect_test "validate record" =
  let check =
    let open Check in
    record (fun fields ->
      let+ name = req fields "name" string
      and+ nickname = opt ~alt:[ "pseudo" ] fields "nickname" string
      and+ age = opt fields "age" int in
      name, nickname, age)
  and repr =
    Repr.(
      record [ "name", string "Mick"; "pseudo", string "msp"; "age", int 30 ])
  in
  dump check repr;
  [%expect {| Ok checked |}]
;;
