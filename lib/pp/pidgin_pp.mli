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

(** {1 S-Expression}

    Pretty-printers for printing S-Expressions and canonical
    S-Expressions. *)

(** Pretty-printer for {!type:Pidgin.Sexp.t} (with
    identation). Generally useful for dumping S-expressions to a
    file. *)
val sexp : Format.formatter -> Pidgin.Sexp.t -> unit

(** Does not produce any special pretty-printing, in order to preserve
    the canonical nature of a canonical S-expression. See
    {!val:Pidgin.Csexp.to_string}. *)
val csexp : Format.formatter -> Pidgin.Csexp.t -> unit
