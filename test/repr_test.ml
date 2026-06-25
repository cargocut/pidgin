(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

open struct
  (* NOTE: [open struct end] allows us to not have an mli and ensure that
     all tests are properly handled. *)

  open Alcotest

  let null0 =
    test_case "null" `Quick (fun () ->
      let expected = Repr.Null
      and computed = Repr.null () in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let bool0 =
    test_case "bool" `Quick (fun () ->
      let expected = Repr.Bool true
      and computed = Repr.bool true in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let bool1 =
    test_case "bool" `Quick (fun () ->
      let expected = Repr.Bool false
      and computed = Repr.bool false in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let int0 =
    test_case "int" `Quick (fun () ->
      let expected = Repr.Int 0
      and computed = Repr.int 0 in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let int1 =
    test_case "int" `Quick (fun () ->
      let expected = Repr.Int 42
      and computed = Repr.int 42 in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let int2 =
    test_case "int" `Quick (fun () ->
      let expected = Repr.Int (-42)
      and computed = Repr.int (-42) in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let float0 =
    test_case "float" `Quick (fun () ->
      let expected = Repr.Float 0.0
      and computed = Repr.float 0.0 in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let float1 =
    test_case "float" `Quick (fun () ->
      let expected = Repr.Float 42.34
      and computed = Repr.float 42.34 in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let float2 =
    test_case "float" `Quick (fun () ->
      let expected = Repr.Float (-42.98776)
      and computed = Repr.float (-42.98776) in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let string0 =
    test_case "string" `Quick (fun () ->
      let expected = Repr.String ""
      and computed = Repr.string "" in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let string1 =
    test_case "string" `Quick (fun () ->
      let expected = Repr.String "Hello World"
      and computed = Repr.string "Hello World" in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let list0 =
    test_case "list" `Quick (fun () ->
      let expected = Repr.List []
      and computed = Repr.list [] in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let list1 =
    test_case "list" `Quick (fun () ->
      let expected =
        Repr.(
          List
            [ Int 32
            ; Float 3.14
            ; Bool true
            ; Null
            ; String "Hello Pidgin"
            ; List [ Int 1; Int 2; Int 3; Int 4; Int 5; Int 6; Int 7 ]
            ; String "foo bar baz"
            ])
      and computed =
        Repr.(
          list
            [ int 32
            ; float 3.14
            ; bool true
            ; null ()
            ; string "Hello Pidgin"
            ; list_of int [ 1; 2; 3; 4; 5; 6; 7 ]
            ; string "foo bar baz"
            ])
      in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let list_of0 =
    test_case "list_of" `Quick (fun () ->
      let expected = Repr.List []
      and computed = Repr.list_of Repr.string [] in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let list_of1 =
    test_case "list_of" `Quick (fun () ->
      let expected = Repr.List [ String "Hello"; String "World" ]
      and computed = Repr.list_of Repr.string [ "Hello"; "World" ] in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let record0 =
    test_case "record" `Quick (fun () ->
      let expected = Repr.Record []
      and computed = Repr.record [] in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let record1 =
    test_case "record" `Quick (fun () ->
      let expected =
        Repr.Record
          [ ( "dummy_list"
            , List
                [ Int 32
                ; Float 3.14
                ; Bool true
                ; Null
                ; String "Hello Pidgin"
                ; List [ Int 1; Int 2; Int 3; Int 4; Int 5; Int 6; Int 7 ]
                ; String "foo bar baz"
                ] )
          ; "is_activated", Bool true
          ; "nickname", String "mspwn"
          ; ( "policies"
            , List [ Int 1; Int 2; Int 3; Int 4; Int 5; Int 6; Int 7; Int 8 ] )
          ]
      and computed =
        Repr.record
          Repr.
            [ ( "Dummy_list"
              , list
                  [ int 32
                  ; float 3.14
                  ; bool true
                  ; null ()
                  ; string "Hello Pidgin"
                  ; list_of int [ 1; 2; 3; 4; 5; 6; 7 ]
                  ; string "foo bar baz"
                  ] )
            ; "is_Activated", bool true
            ; "Nickname", string "mspwn"
            ; "   policies  ", list_of int [ 1; 2; 3; 4; 5; 6; 7; 8 ]
            ]
      in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;

  let record2 =
    test_case "record" `Quick (fun () ->
      let expected =
        Repr.Record
          [ ( "Dummy_list"
            , List
                [ Int 32
                ; Float 3.14
                ; Bool true
                ; Null
                ; String "Hello Pidgin"
                ; List [ Int 1; Int 2; Int 3; Int 4; Int 5; Int 6; Int 7 ]
                ; String "foo bar baz"
                ] )
          ; "is_Activated", Bool true
          ; "Nickname", String "mspwn"
          ; ( "   policies  "
            , List [ Int 1; Int 2; Int 3; Int 4; Int 5; Int 6; Int 7; Int 8 ] )
          ]
      and computed =
        Repr.record
          ~normalize_keys:false
          Repr.
            [ ( "Dummy_list"
              , list
                  [ int 32
                  ; float 3.14
                  ; bool true
                  ; null ()
                  ; string "Hello Pidgin"
                  ; list_of int [ 1; 2; 3; 4; 5; 6; 7 ]
                  ; string "foo bar baz"
                  ] )
            ; "is_Activated", bool true
            ; "Nickname", string "mspwn"
            ; "   policies  ", list_of int [ 1; 2; 3; 4; 5; 6; 7; 8 ]
            ]
      in
      check Test_lib.Testable.repr "should be equal" expected computed)
  ;;
end

let cases =
  ( "Repr"
  , [ null0
    ; bool0
    ; bool1
    ; int0
    ; int1
    ; int2
    ; float0
    ; float1
    ; float2
    ; string0
    ; string1
    ; list0
    ; list1
    ; list_of0
    ; list_of1
    ; record0
    ; record1
    ; record2
    ] )
;;
