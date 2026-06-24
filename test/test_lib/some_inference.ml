(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

let dump_kind_of expr =
  expr |> Kind.infer |> Format.asprintf "%a" Pp.kind |> print_endline
;;

let%expect_test "infer" =
  dump_kind_of Repr.(null ());
  [%expect {| null |}]
;;

let%expect_test "infer" =
  dump_kind_of Repr.(bool true);
  [%expect {| bool |}]
;;

let%expect_test "infer" =
  dump_kind_of Repr.(bool true);
  [%expect {| bool |}]
;;

let%expect_test "infer" =
  dump_kind_of Repr.(int 42);
  [%expect {| int |}]
;;

let%expect_test "infer" =
  dump_kind_of Repr.(float 42.3);
  [%expect {| float |}]
;;

let%expect_test "infer" =
  dump_kind_of Repr.(string "hello World");
  [%expect {| string |}]
;;

let%expect_test "infer" =
  (* NOTE: We don not unify sum inside pair for simplicity reasons. *)
  dump_kind_of
    Repr.(
      record
        [ "foo", string "hello"
        ; "policies", list_of int [ 1; 2; 3; 4 ]
        ; "unified_list", list []
        ; "is_valid", bool true
        ; "state", result ~ok:int ~error:string (Ok 12)
        ; ( "more"
          , record
              [ ( "things"
                , list_of
                    (pair int (either ~left:string ~right:int))
                    [ 1, Right 12; 2, Left "foo" ] )
              ] )
        ]);
  [%expect
    {|
    {"foo": string, "policies": [int], "unified_list": [?any], "is_valid": bool,
     "state": #ok<int>,
     "more": {"things": [((int * #right<int>) | (int * #left<string>))]}}
    |}]
;;
