(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

open struct
  (* NOTE: [open struct end] allows us to not have an mli and ensure that
     all tests are properly handled. *)

  open Alcotest
  open Test_lib.Archetypes

  let human0 =
    test_case "human" `Quick (fun () ->
      let repr =
        let open Repr in
        record [ "nickname", string "mspwn"; "gender", string "male" ]
      in
      let expected =
        Human.make ~nickname:"mspwn" ~gender:Gender.male () |> Result.ok
      and computed = repr |> Human.from_pidgin in
      check
        (Test_lib.Testable.checked Human.testable)
        "should be equal"
        expected
        computed)
  ;;

  let human1 =
    test_case "human" `Quick (fun () ->
      let repr = Repr.record [] in
      let expected =
        Error.Check.invalid_record
          repr
          (Nel.append
             (Error.Check.missing_field ~alt:[ "nick"; "pseudo" ] "nickname")
             (Error.Check.missing_field "gender"))
        |> Result.error
      and computed = repr |> Human.from_pidgin in
      check
        (Test_lib.Testable.checked Human.testable)
        "should be equal"
        expected
        computed)
  ;;

  let human2 =
    test_case "human" `Quick (fun () ->
      let repr =
        let open Repr in
        record
          [ "nickname", string "mspwn"
          ; "gender", string "male"
          ; "first_name", string "Mick"
          ; "lastname", string "Spawn"
          ; "age", int 30
          ]
      in
      let expected =
        Human.make
          ~nickname:"mspwn"
          ~firstname:"Mick"
          ~lastname:"Spawn"
          ~age:30
          ~gender:Gender.male
          ()
        |> Result.ok
      and computed = repr |> Human.from_pidgin in
      check
        (Test_lib.Testable.checked Human.testable)
        "should be equal"
        expected
        computed)
  ;;

  let human3 =
    test_case "human" `Quick (fun () ->
      let repr =
        let open Repr in
        record
          [ "niname", string "mspwn"
          ; "gender", string "my own gender"
          ; "first_name", string "Mick"
          ; "lastname", string "Spawn"
          ; "age", string "trente"
          ]
      in
      let expected =
        Error.Check.invalid_record
          repr
          (Nel.append
             (Error.Check.missing_field ~alt:[ "nick"; "pseudo" ] "nickname")
             (Error.Check.invalid_field "age"
              @@ Error.Check.unexpected_kind Kind.int (Repr.string "trente")))
        |> Result.error
      and computed = repr |> Human.from_pidgin in
      check
        (Test_lib.Testable.checked Human.testable)
        "should be equal"
        expected
        computed)
  ;;

  let user0 =
    test_case "user" `Quick (fun () ->
      let repr =
        let open Repr in
        record
          [ "nickname", string "mspwn"
          ; "gender", string "male"
          ; "first_name", string "Mick"
          ; "lastname", string "Spawn"
          ; "age", int 30
          ; "email", string "msp@domain.com"
          ]
      in
      let expected =
        User.make
          ~email:"msp@domain.com"
          ~level:0
          ~human:
            (Human.make
               ~nickname:"mspwn"
               ~firstname:"Mick"
               ~lastname:"Spawn"
               ~age:30
               ~gender:Gender.male
               ())
          ()
        |> Result.ok
      and computed = repr |> User.from_pidgin in
      check
        (Test_lib.Testable.checked User.testable)
        "should be equal"
        expected
        computed)
  ;;

  let user1 =
    test_case "user" `Quick (fun () ->
      let repr =
        let open Repr in
        record
          [ "nickname", string "mspwn"
          ; "gender", string "male"
          ; "first_name", string "Mick"
          ; "lastname", string "Spawn"
          ; "age", int 30
          ; "email", string "msp@domain.com"
          ; "level", int 10
          ]
      in
      let expected =
        User.make
          ~email:"msp@domain.com"
          ~level:10
          ~human:
            (Human.make
               ~nickname:"mspwn"
               ~firstname:"Mick"
               ~lastname:"Spawn"
               ~age:30
               ~gender:Gender.male
               ())
          ()
        |> Result.ok
      and computed = repr |> User.from_pidgin in
      check
        (Test_lib.Testable.checked User.testable)
        "should be equal"
        expected
        computed)
  ;;

  let user2 =
    test_case "user" `Quick (fun () ->
      let repr =
        let open Repr in
        record
          [ "nickname", string "mspwn"
          ; "gender", string "male"
          ; "email", string "msp@domain.com"
          ; "level", int 10
          ]
      in
      let expected =
        User.make
          ~email:"msp@domain.com"
          ~level:10
          ~human:(Human.make ~nickname:"mspwn" ~gender:Gender.male ())
          ()
        |> Result.ok
      and computed = repr |> User.from_pidgin in
      check
        (Test_lib.Testable.checked User.testable)
        "should be equal"
        expected
        computed)
  ;;

  let user3 =
    test_case "user" `Quick (fun () ->
      let repr =
        let open Repr in
        record
          [ "nickname", string "mspwn"
          ; "first_name", string "Mick"
          ; "lastname", string "Spawn"
          ; "age", int 30
          ]
      in
      let expected =
        Error.Check.invalid_record
          repr
          (Nel.append
             (Error.Check.invalid_subrecord
                (Error.Check.invalid_record
                   repr
                   (Error.Check.missing_field "gender")))
             (Error.Check.missing_field ~alt:[ "mail" ] "email"))
        |> Result.error
      and computed = repr |> User.from_pidgin in
      check
        (Test_lib.Testable.checked User.testable)
        "should be equal"
        expected
        computed)
  ;;
end

let cases =
  ( "Check (Record)"
  , [ human0; human1; human2; human3; user0; user1; user2; user3 ] )
;;
