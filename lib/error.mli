(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

module Check : sig
  (** Describes all the errors that can occur when validating arbitrary
      data from the {!type:Repr.t} format. *)

  (** {1 Types}

      Errors are divided into several distinct categories:

      - Validation errors for {b values}: all errors that may occur when
        attempting to validate arbitrary data.

      - Validation error for {b records}: all errors that may occur when
        attempting to validate a record (including value errors). *)

  (** Errors for values. *)
  type for_value =
    | Unexpected_kind of
        { expected : Kind.t
        ; given : Kind.t
        ; value : Repr.t
        }
    | Invalid_list of
        { errors : (int * for_value) Nel.t
        ; value : Repr.t
        }
    | Invalid_record of
        { errors : for_record Nel.t
        ; value : Repr.t
        }
    | Unexpected_value of
        { value : Repr.t option
        ; message : string
        } (** Used for custom error messages. *)

  (** Errors for records. *)
  and for_record =
    | Invalid_field of
        { field : string Nel.t
        ; error : for_value
        }
    | Missing_field of string Nel.t
    | Invalid_subrecord of for_value

  (** {1 Smart Constructors} *)

  (** Create an [Unexpected_kind] error. *)
  val unexpected_kind : Kind.t -> Repr.t -> for_value

  (** Used for a custom error message. *)
  val unexpected_value : ?value:Repr.t -> string -> for_value

  (** Create an [Invalid_list] error. *)
  val invalid_list : Repr.t -> (int * for_value) Nel.t -> for_value

  (** Create a [Missing_field] error. *)
  val missing_field : ?alt:string list -> string -> for_record Nel.t

  (** Create a [Invalid_field] error. *)
  val invalid_field
    :  ?alt:string list
    -> string
    -> for_value
    -> for_record Nel.t

  (** Create a [Invalid_subrecord] error. *)
  val invalid_subrecord : for_value -> for_record Nel.t

  (** Create a [Invalid_record] error. *)
  val invalid_record : Repr.t -> for_record Nel.t -> for_value

  (** {1 Equalities} *)

  (** Equality between value errors. *)
  val equal_for_value : for_value -> for_value -> bool

  (** Equality between record errors. *)
  val equal_for_record : for_record -> for_record -> bool

  (** See {!val:equal_for_value}. *)
  val equal : for_value -> for_value -> bool
end

module Sexp : sig
  (** Describes all the errors that can occur when parsing S-expression. *)

  (** {1 Types} *)

  (** Errors for S-Expression. *)
  type t = Non_terminated_node of int

  (** {1 Smart Constructors} *)

  (** Create a [Non_terminated_node] error. *)
  val non_terminated_node : int -> ('a, t) result

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
        }
    | Expected_atom of int
    | Expected_number_or_atom of int
    | Expected_number of int
    | Unexpected_char of char * int
    | Non_terminated_node of int

  (** {1 Smart Constructors} *)

  (** Create a [Premature_end_of_atom] error. *)
  val premature_end_of_atom
    :  expected_length:int
    -> given_length:int
    -> ('a, t) result

  (** {1 Equalities} *)

  (** Equality between Canonical Sexp Parsing errors. *)
  val equal : t -> t -> bool
end
