(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

(** Pre-built {{!module:Intf} drivers} to handle common OCaml library
    formats. *)

(** {1 Types} *)

module type SOURCE = Intf.SOURCE
module type TARGET = Intf.TARGET

(** {1 Pre-build Drivers} *)

module Sexp : sig
  (** Dealing with the {!module:Sexp} format. *)

  (** Since {!module:Csexp} shares the same representation as
      {!module:Sexp}, the driver supports both. *)

  include SOURCE with type t = Sexp.t
  include TARGET with type t := t
end

module Yojson : sig
  (** Dealing with the Yojson (basic) format. *)

  type t =
    [ `Null
    | `Bool of bool
    | `Int of int
    | `Float of float
    | `String of string
    | `Assoc of (string * t) list
    | `List of t list
    ]

  (** Since {{:https://github.com/ocaml-community/yojson} Yojson} use
      polymoprhic variant as a type definiton, we do not depend on the
      library to support it. *)

  include SOURCE with type t := t
  include TARGET with type t := t
end

module Ezjsonm : sig
  (** Dealing with the Ezjonm format (which is also used, notably, by
      {{:https://github.com/avsm/ocaml-yaml} YAML}). *)

  type t =
    [ `Null
    | `Bool of bool
    | `Float of float
    | `String of string
    | `A of t list
    | `O of (string * t) list
    ]

  (** Since {{:https://github.com/mirage/ezjsonm} Ezjsonm} use
      polymoprhic variant as a type definiton, we do not depend on the
      library to support it. *)

  include SOURCE with type t := t
  include TARGET with type t := t
end
