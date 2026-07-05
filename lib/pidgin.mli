(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

(** [Pidgin] is a generic key-value data structure description
    language that enables fine-grained validation to deal with format
    like JSON, S-expression, Yaml, ToML etc.

    The main idea is to present a minimal representation (very similar
    to that of JSON) and to provide:

    - a DSL for describing arbitrary data structures in this language
      (see {!module:Repr})
    - Validation functions that operate on data described using this
      DSL (see {!module:Check})
    - A bidirectional conversion approach (see {!module:Driver}),
      imposing a cost due to the indirect nature of the generic format
      (though it is viable in many scenarios).

    [Pidgin] does not statically preserve the type of expressions;
    instead, it hides them, which allows expressions written in this
    language to be treated as an untyped runtime representation of
    arbitrary OCaml values (enabling the derivation of pretty-printers
    and equality functions, for example). *)

(** {1 Data Representation} *)

module Repr = Repr
module Kind = Kind

(** {1 Data Validation} *)

module Check = Check

(** {1 Drivers} *)

module Driver = Driver

(** {1 S-Expression}

    S-expressions make it easy to serialize Pidgin expressions;
    however, the format was designed to be used with any key-value
    representation. *)

module Sexp = Sexp
module Csexp = Csexp

(** {1 Misc} *)

module Misc = Misc
