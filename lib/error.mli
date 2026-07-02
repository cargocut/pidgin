(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

module Sexp : sig
  (** Describes all the errors that can occur when parsing S-expression. *)

  (** {1 Types} *)

  (** Errors for S-Expression. *)
  type t =
    | Non_terminated_node of int
    | Non_opened_node of int

  (** {1 Smart Constructors} *)

  (** Create a [Non_terminated_node] error. *)
  val non_terminated_node : int -> ('a, t) result

  (** Create a [Non_opened_node] error. *)
  val non_opened_node : int -> ('a, t) result

  (** {1 Equalities} *)

  (** Equality between Sexp Parsing errors. *)
  val equal : t -> t -> bool
end

module Csexp : sig
  (** Describes all the errors that can occur when parsing Canonical
      S-expression. *)

  (** Errors for Canonical S-Expression parsing. *)
  type t =
    | Premature_end_of_atom of
        { expected_length : int
        ; given_length : int
        ; position : int
        }
    | Expected_atom of int
    | Expected_number_or_column of int
    | Expected_number of int
    | Unexpected_char of char * int
    | Non_terminated_node of int
    | Non_opened_node of int

  (** {1 Smart Constructors} *)

  (** Create a [Premature_end_of_atom] error. *)
  val premature_end_of_atom
    :  expected_length:int
    -> given_length:int
    -> int
    -> ('a, t) result

  (** Create a [Premature_end_of_atom] error. *)
  val expected_atom : int -> ('a, t) result

  (** Create a [Expected_number_or_column] error. *)
  val expected_number_or_column : int -> ('a, t) result

  (** Create a [Expected_number] error. *)
  val expected_number : int -> ('a, t) result

  (** Create a [Premature_end_of_atom] error. *)
  val unexpected_char : char -> int -> ('a, t) result

  (** Create a [Non_terminated_node] error. *)
  val non_terminated_node : int -> ('a, t) result

  (** Create a [Non_opened_node] error. *)
  val non_opened_node : int -> ('a, t) result

  (** {1 Equalities} *)

  (** Equality between Canonical Sexp Parsing errors. *)
  val equal : t -> t -> bool
end
