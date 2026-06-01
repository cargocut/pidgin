(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

(* Temporary *)
type value_error =
  | Invalid_shape of
      { expected : string
      ; given : Data.t
      }
  | Invalid_list of
      { errors : (int * value_error) list
      ; given : Data.t list
      }

type 'a t = ('a, value_error) result

val null : Data.t -> unit t
val bool : Data.t -> bool t
val int : Data.t -> int t
val float : Data.t -> float t
val string : ?strict:bool -> Data.t -> string t
val list_of : (Data.t -> 'a t) -> Data.t -> 'a list t
val record_of : (Data.t -> 'a t) -> Data.t -> (string * 'a) list t
