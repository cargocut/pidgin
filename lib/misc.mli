(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

(** [strim s] apply [trim] and [lowercase_ascii] on the given [s]. *)
val strim : string -> string

(** [find_assoc ?normalize_keys key assoc] try to find the given [key]
    in the [assoc] list. If [normalize_keys] is [true] (default
    choice) it will trim and lowercase the key. *)
val find_assoc
  :  ?normalize_keys:bool
  -> (string * 'a) list
  -> string
  -> 'a option
