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

(** Errors that may occur during parsing. *)
type parsing_error =
  | Non_terminated_node of int
  | Non_opened_node of int

(** Describe a parsed S-Expression. *)
type parsed = (t, parsing_error) result

(** {1 Building} *)

(** [atom x] create an atom. *)
val atom : string -> t

(** [node xs] create a node. *)
val node : t list -> t

(** {1 Parsing} *)

(** [from_seq charseq] parse a sequence of chars ([charseq]) as
    S-Expression. *)
val from_seq : char Seq.t -> parsed

(** [from_string string] parse a string as a S-Expression. *)
val from_string : string -> parsed

(** {1 Conversion} *)

(** [to_buffer buf sexp] output the given [sexp] into the given
    [buffer]. *)
val to_buffer : Buffer.t -> t -> unit

(** [to_string sexp] render a given [sexp] as a [string]. Note that
    the function does not perform pretty-printing. *)
val to_string : t -> string

(** {1 Equality} *)

(** Equality between S-Expressions. *)
val equal : t -> t -> bool

(** {1 Drivers} *)

(** S-expression can be use as a target. *)
include Intf.TARGET with type t := t

(** S-expression can be use as a source. *)
include Intf.SOURCE with type t := t
