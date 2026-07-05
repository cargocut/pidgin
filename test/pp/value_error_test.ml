(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

open Pidgin
open Check

let dump err =
  err |> Format.asprintf "%a" Pidgin_pp.value_error |> print_endline
;;

let%expect_test "dump value error without value" =
  dump
    (Unexpected_value
       { value = None; message = "an error is occured, it is embarassing" });
  [%expect {| Unexpected value:an error is occured, it is embarassing |}]
;;

let%expect_test "dump value error with simple value" =
  dump
    (Unexpected_value
       { value = Some (Repr.string "Hello World")
       ; message = "an error is occured, it is embarassing"
       });
  [%expect
    {| Unexpected value ("Hello World"):an error is occured, it is embarassing |}]
;;

let%expect_test "dump value error withs a very complicated value" =
  dump
    (Unexpected_value
       { value =
           Some
             Repr.(
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
                        ~left:
                          (result
                             ~ok:(either ~left:int ~right:string)
                             ~error:string)
                        ~right:string)
                     [ Left (Ok (Right "foo"))
                     ; Right "foo"
                     ; Left (Error "Hello")
                     ; Left (Ok (Right "World"))
                     ]
                 ; triple int string bool (43, "Hello World", true)
                 ; triple int string bool (23, "Hello", false)
                 ])
       ; message = "an error is occured, it is embarassing"
       });
  [%expect
    {|
    Unexpected value
    ([{"nickname": "mspwn",
        "gender": "male",
        "email": "msp@domain.com",
        "other":
         {"nickname": "mspwn",
           "gender": "male",
           "email": "msp@domain.com",
           "level": 10,
           "more_nesting":
            {"nickname": "mspwn",
              "gender": "male",
              "email": "msp@domain.com",
              "level": 10}},
        "level": 10}, 45, "Hello World",
      [{"constr": "left",
         "value": {"constr": "ok",
                    "value": {"constr": "right",
                               "value": "foo"}}},
       {"constr": "right",
         "value": "foo"},
       {"constr": "left",
         "value": {"constr": "error",
                    "value": "Hello"}},
       {"constr": "left",
         "value":
          {"constr": "ok",
            "value": {"constr": "right",
                       "value": "World"}}}],
      {"first": 43,
        "second": {"first": "Hello World",
                    "second": true}},
      {"first": 23,
        "second": {"first": "Hello",
                    "second": false}}]):
    an error is occured, it is embarassing
    |}]
;;

let%expect_test "dump unexpected kind" =
  dump
    (Unexpected_kind
       { value = Repr.int 10; expected = Kind.string; given = Kind.int });
  [%expect
    {|
    Unexpected kind for (10).
    Expected: string
    Given: int
    |}]
;;

let%expect_test "dump unexepcted kind with long kind" =
  let big_record =
    Repr.(
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
        ])
  in
  dump
    (Unexpected_kind
       { value = Repr.int 10
       ; expected = Kind.string
       ; given = Kind.infer big_record
       });
  [%expect
    {|
    Unexpected kind for (10).
    Expected: string
    Given: [(int | string
            | [(#left<#error<string>> | #left<#ok<#right<string>>>
               | #right<string>)] | (int * (string * bool))
            | {"nickname": string,
                "gender": string,
                "email": string,
                "other":
                 {"nickname": string,
                   "gender": string,
                   "email": string,
                   "level": int,
                   "more_nesting":
                    {"nickname": string,
                      "gender": string,
                      "email": string,
                      "level": int}},
                "level": int})]
    |}]
;;

let%expect_test "dump unexepcted kind with long kind and long value" =
  let big_record =
    Repr.(
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
        ])
  in
  dump
    (Unexpected_kind
       { value = big_record
       ; expected = Kind.string
       ; given = Kind.infer big_record
       });
  [%expect
    {|
    Unexpected kind for ([{"nickname": "mspwn",
                            "gender": "male",
                            "email": "msp@domain.com",
                            "other":
                             {"nickname": "mspwn",
                               "gender": "male",
                               "email": "msp@domain.com",
                               "level": 10,
                               "more_nesting":
                                {"nickname": "mspwn",
                                  "gender": "male",
                                  "email": "msp@domain.com",
                                  "level": 10}},
                            "level": 10}, 45, "Hello World",
                          [{"constr": "left",
                             "value":
                              {"constr": "ok",
                                "value": {"constr": "right",
                                           "value": "foo"}}},
                           {"constr": "right",
                             "value": "foo"},
                           {"constr": "left",
                             "value": {"constr": "error",
                                        "value": "Hello"}},
                           {"constr": "left",
                             "value":
                              {"constr": "ok",
                                "value": {"constr": "right",
                                           "value": "World"}}}],
                          {"first": 43,
                            "second": {"first": "Hello World",
                                        "second": true}},
                          {"first": 23,
                            "second": {"first": "Hello",
                                        "second": false}}]).
    Expected: string
    Given: [(int | string
            | [(#left<#error<string>> | #left<#ok<#right<string>>>
               | #right<string>)] | (int * (string * bool))
            | {"nickname": string,
                "gender": string,
                "email": string,
                "other":
                 {"nickname": string,
                   "gender": string,
                   "email": string,
                   "level": int,
                   "more_nesting":
                    {"nickname": string,
                      "gender": string,
                      "email": string,
                      "level": int}},
                "level": int})]
    |}]
;;
