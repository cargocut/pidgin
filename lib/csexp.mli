(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

(** A minimalist library for describing and parsing
    {{:https://en.wikipedia.org/wiki/Canonical_S-expressions}
    Canonical S-expression}. *)

(** {1 Types} *)

(** Type is shared with {!module:Sexp}. *)
type t = Sexp.t =
  | Atom of string
  | Node of t list

(** Describe a parsed Canonical S-Expression. *)
type parsed = (t, Error.Csexp.t) result

(** {1 Parsing} *)

(** [from_seq charseq] parse a sequence of chars ([charseq]) as
    Canonical S-Expression. *)
val from_seq : char Seq.t -> parsed

(** [from_string string] parse a string as a Canonical
    S-Expression. *)
val from_string : string -> parsed

(** {1 Conversion} *)

(** [to_buffer buf csexp] output the given [csexp] into the given
    [buffer]. *)
val to_buffer : Buffer.t -> t -> unit

(** [to_string csexp] render a given [csexp] as a [string]. Note that
    the function does not perform pretty-printing. *)
val to_string : t -> string

(** {1 Equality} *)

(** Equality between Canonical S-Expressions. *)
val equal : t -> t -> bool
