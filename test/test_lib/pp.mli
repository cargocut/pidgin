(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

(** Some pretty-printers *)

(** A pretty printer for [Repr] that looks like JSON, for debugging
    puprose. *)
val repr : Format.formatter -> Pidgin.Repr.t -> unit

(** A pretty-printer for kinds. *)
val kind : Format.formatter -> Kind.t -> unit

(** A pretty-printer for Sexp. *)
val sexp : Format.formatter -> Sexp.t -> unit

(** A pretty-printer for Nel. *)
val nel
  :  (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a Nel.t
  -> unit

(** A pretty-printer for value_error. *)
val check_error : Format.formatter -> Check.value_error -> unit

(** A pretty-printer for sexp_parsing. *)
val sexp_parsing_error : Format.formatter -> Sexp.parsing_error -> unit

(** A pretty-printer for csexp_parsing. *)
val csexp_parsing_error : Format.formatter -> Csexp.parsing_error -> unit

(** A pretty-printer for checked values. *)
val checked_value
  :  (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a Check.value
  -> unit

(** A pretty-printer for checked sexp. *)
val sexp_parsed : Format.formatter -> Sexp.parsed -> unit

(** A pretty-printer for checked csexp. *)
val csexp_parsed : Format.formatter -> Csexp.parsed -> unit
