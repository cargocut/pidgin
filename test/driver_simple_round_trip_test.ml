(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

open struct
  (* NOTE: [open struct end] allows us to not have an mli and ensure that
     all tests are properly handled. *)

  open Alcotest
  open Test_lib.Archetypes

  (* NOTE: A simple test that ensure the coersion
     seems... consistent. *)
  let user0 =
    test_case "user" `Quick (fun () ->
      let repr =
        let open Repr in
        record
          [ "nickname", string "mspwn"
          ; "gender", string "male"
          ; "email", string "msp@domain.com"
          ; "level", int 10
          ]
        |> Sexp.translate_from_pidgin
        |> Sexp.translate_to_pidgin
        |> Driver.Yojson.translate_from_pidgin
        |> Driver.Yojson.translate_to_pidgin
        |> Driver.Ezjsonm.translate_from_pidgin
        |> Driver.Ezjsonm.translate_to_pidgin
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
end

let cases = "Driver (Some simple round trip)", [ user0 ]
