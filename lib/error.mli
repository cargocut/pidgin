(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

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
  | Unexpected_value of string (** Used for custom error messages. *)

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
val unexpected_value : string -> for_value

(** Create an [Invalid_list] error. *)
val invalid_list : Repr.t -> (int * for_value) Nel.t -> for_value

(** Create a [Missing_field] error. *)
val missing_field : ?alt:string list -> string -> for_record Nel.t

(** Create a [Invalid_field] error. *)
val invalid_field : ?alt:string list -> string -> for_value -> for_record Nel.t

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
