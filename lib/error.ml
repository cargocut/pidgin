(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

type for_value =
  | Unexpected_kind of
      { expected : Kind.t
      ; given : Kind.t
      ; value : Repr.t
      }
  | Invalid_list of
      { errors : (int * for_value) Nel.t
      ; value : Repr.t
      }
  | Invalid_record of
      { errors : for_record Nel.t
      ; value : Repr.t
      }
  | Unexpected_value of string

and for_record =
  | Invalid_field of
      { field : string Nel.t
      ; error : for_value
      }
  | Missing_field of string Nel.t
  | Invalid_subrecord of for_value

let unexpected_kind expected value =
  let given = Kind.infer value in
  Unexpected_kind { given; expected; value }
;;

let invalid_list value errors = Invalid_list { errors; value }

let missing_field ?(alt = []) field =
  Nel.singleton @@ Missing_field Nel.(make field alt)
;;

let invalid_field ?(alt = []) field error =
  Nel.singleton @@ Invalid_field { field = Nel.(make field alt); error }
;;

let invalid_record value errors = Invalid_record { errors; value }
let invalid_subrecord err = Nel.singleton @@ Invalid_subrecord err
