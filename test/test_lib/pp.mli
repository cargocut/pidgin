(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

(** Some pretty-printers *)

(** A pretty printer for [Repr] that looks like JSON, for debugging
    puprose. *)
val repr : Format.formatter -> Pidgin.Repr.t -> unit

(** A pretty-printer for kinds. *)
val kind : Format.formatter -> Kind.t -> unit

(** A pretty-printer for value_error. *)
val error_for_value : Format.formatter -> Error.for_value -> unit

(** A pretty-printer for checked values. *)
val checked_value
  :  (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a Check.value
  -> unit
