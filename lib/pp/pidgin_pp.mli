(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

(** A set of pretty-printers to handle the different types defined in
    the Pidgin library. *)

(** {1 Internal}

    Pretty-printers, primarily for inspection, that describe Pidgin's
    internal elements. *)

(** Pretty-printer for {!type:Pidgin.Repr.t}. Uses a representation
    similar to JSON. *)
val repr : Format.formatter -> Pidgin.Repr.t -> unit

(** Pretty-printer for {!type:Pidgin.Kind.t}. Try to make the Kinds
    readable.. *)
val kind : Format.formatter -> Pidgin.Kind.t -> unit
