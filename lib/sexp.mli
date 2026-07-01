(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

(** A minimalist library for describing and parsing
    {{:https://en.wikipedia.org/wiki/S-expression} S-expression}. *)

(** {1 Types}

    An S-Expression is a data structure with an extremely simple
    recursive syntax tree. It is made up of [atoms] (strings) and
    [nodes] (also S-Expressions). *)

(** S-Expression AST. *)
type t =
  | Atom of string
  | Node of t list

(** Describe a parsed S-Expression. *)
type parsed = (t, Error.sexp_parsing) result

(** {1 Parsing} *)

(** [from_seq charseq] parse a sequence of chars ([charseq]) as
    S-Expression. *)
val from_seq : char Seq.t -> parsed

(** [from_string string] parse a string as S-Expression. *)
val from_string : string -> parsed

(** {1 Conversion} *)

(** [to_string sexp] render a given [sexp] as a [string]. Note that
    the function does not perform pretty-printing. *)
val to_string : t -> string

(** {1 Equality} *)

(** Equality between S-Expressions. *)
val equal : t -> t -> bool
