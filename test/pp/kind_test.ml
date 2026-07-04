(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

open Pidgin.Kind

let dump repr = repr |> Format.asprintf "%a" Pidgin_pp.kind |> print_endline

let%expect_test "dump any" =
  dump any;
  [%expect {| any |}]
;;

let%expect_test "dump null" =
  dump null;
  [%expect {| null |}]
;;

let%expect_test "dump bool" =
  dump bool;
  [%expect {| bool |}]
;;

let%expect_test "dump int" =
  dump int;
  [%expect {| int |}]
;;

let%expect_test "dump float" =
  dump float;
  [%expect {| float |}]
;;

let%expect_test "dump string" =
  dump string;
  [%expect {| string |}]
;;

let%expect_test "dump list" =
  dump (list int);
  [%expect {| [int] |}]
;;

let%expect_test "dump pair" =
  dump (pair string @@ list int);
  [%expect {| (string * [int]) |}]
;;

let%expect_test "dump a record" =
  let r =
    let open Pidgin.Repr in
    list
      [ record
          [ "nickname", string "mspwn"
          ; "gender", string "male"
          ; "email", string "msp@domain.com"
          ; ( "other"
            , record
                [ "nickname", string "mspwn"
                ; "gender", string "male"
                ; "email", string "msp@domain.com"
                ; "level", int 10
                ; ( "more_nesting"
                  , record
                      [ "nickname", string "mspwn"
                      ; "gender", string "male"
                      ; "email", string "msp@domain.com"
                      ; "level", int 10
                      ] )
                ] )
          ; "level", int 10
          ]
      ; int 45
      ; string "Hello World"
      ; list_of
          (either
             ~left:(result ~ok:(either ~left:int ~right:string) ~error:string)
             ~right:string)
          [ Left (Ok (Right "foo"))
          ; Right "foo"
          ; Left (Error "Hello")
          ; Left (Ok (Right "World"))
          ]
      ; triple int string bool (43, "Hello World", true)
      ; triple int string bool (23, "Hello", false)
      ]
  in
  dump (infer r);
  [%expect
    {|
    [(int | string
      | [(#left<#error<string>> | #left<#ok<#right<string>>> | #right<string>)]
      | (int * (string * bool))
      | {"nickname": string, "gender": string, "email": string,
         "other":
          {"nickname": string, "gender": string, "email": string, "level": int,
           "more_nesting":
            {"nickname": string, "gender": string, "email": string, "level": int}},
         "level": int})]
    |}]
;;
