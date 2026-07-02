(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

module Sexp = struct
  type t =
    | Non_terminated_node of int
    | Non_opened_node of int

  let equal a b =
    match a, b with
    | Non_terminated_node a, Non_terminated_node b
    | Non_opened_node a, Non_opened_node b -> Int.equal a b
    | Non_terminated_node _, _ | Non_opened_node _, _ -> false
  ;;

  let non_terminated_node pos = Error (Non_terminated_node pos)
  let non_opened_node pos = Error (Non_opened_node pos)
end

module Csexp = struct
  type t =
    | Premature_end_of_atom of
        { expected_length : int
        ; given_length : int
        ; position : int
        }
    | Expected_atom of int
    | Expected_number_or_column of int
    | Expected_number of int
    | Unexpected_char of char * int
    | Non_terminated_node of int
    | Non_opened_node of int

  let equal a b =
    match a, b with
    | ( Premature_end_of_atom { expected_length; given_length; position }
      , Premature_end_of_atom b ) ->
      Int.equal position b.position
      && Int.equal expected_length b.expected_length
      && Int.equal given_length b.given_length
    | Expected_atom a, Expected_atom b
    | Expected_number a, Expected_number b
    | Expected_number_or_column a, Expected_number_or_column b
    | Non_terminated_node a, Non_terminated_node b
    | Non_opened_node a, Non_opened_node b -> Int.equal a b
    | Premature_end_of_atom _, _
    | Expected_atom _, _
    | Expected_number_or_column _, _
    | Expected_number _, _
    | Unexpected_char (_, _), _
    | Non_terminated_node _, _
    | Non_opened_node _, _ -> false
  ;;

  let premature_end_of_atom ~expected_length ~given_length position =
    Error (Premature_end_of_atom { expected_length; given_length; position })
  ;;

  let expected_atom pos = Error (Expected_atom pos)
  let expected_number_or_column pos = Error (Expected_number_or_column pos)
  let expected_number pos = Error (Expected_number pos)
  let unexpected_char c pos = Error (Unexpected_char (c, pos))
  let non_terminated_node pos = Error (Non_terminated_node pos)
  let non_opened_node pos = Error (Non_opened_node pos)
end
