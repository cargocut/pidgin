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

(** [escape_spaces s] escape spaces for the given string [s]. *)
val escape_spaces : string -> string

(** [concat_with ~sep f xs] concat every elts of [xs] with [sep] using
    [f] on each element. *)
val concat_with : sep:string -> ('a -> string) -> 'a list -> string

(** See {!val:concat_with} buf acting on Buffer. *)
val concat_buffer_with
  :  sep:string
  -> Buffer.t
  -> (Buffer.t -> 'a -> unit)
  -> 'a list
  -> unit
