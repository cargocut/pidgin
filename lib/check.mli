(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

(** Describes a validation tree for validating complex structured data
    from {!type:Repr.t}. *)

(** {1 Types} *)

(** {2 Errors}

    Errors are divided into several distinct categories:

    - Validation errors for {b values}: all errors that may occur
      when attempting to validate arbitrary data.

    - Validation error for {b records}: all errors that may occur
      when attempting to validate a record (including value errors). *)

(** Describes all the errors that can occur when validating arbitrary
    data from the {!type:Repr.t} format. *)
type value_error =
  | Unexpected_kind of
      { expected : Kind.t
      ; given : Kind.t
      ; value : Repr.t
      }
  | Invalid_list of
      { errors : (int * value_error) Nel.t
      ; value : Repr.t
      }
  | Invalid_record of
      { errors : record_error Nel.t
      ; value : Repr.t
      }
  | Unexpected_value of
      { value : Repr.t option
      ; message : string
      }

(** Errors for record. *)
and record_error =
  | Invalid_field of
      { field : string Nel.t
      ; error : value_error
      }
  | Missing_field of string Nel.t
  | Invalid_subrecord of value_error

(** {2 Shortcuts} *)

(** A type describing a value that has been validated. *)
type 'a value = ('a, value_error) result

(** A type describing a record that has been validated. *)
type 'a record = ('a, record_error Nel.t) result

(** A type describing a function that perform validation (a check). *)
type ('a, 'b) fn = 'a -> 'b value

(** A type describing a function that perform validation (a check)
    from a {!type:Repr.t}. *)
type 'a t = (Repr.t, 'a) fn

module type CHECKABLE = sig
  (** Describes a module whose values of type {!type:t} can be
      checked from a Pidgin representation ({!type:Repr.t}) *)

  (** The type that can be checked *)
  type t

  (** [from_pidgin repr] check [repr] into [t]. *)
  val from_pidgin : (Repr.t, t) fn
end

(** {1 Data Validation} *)

(** [const x] always succeed with [x]. *)
val const : 'a -> ('b, 'a) fn

(** Validator from {!type:Repr.t} to [unit]. *)
val null : unit t

(** Validator from {!type:Repr.t} to [bool]. *)
val bool : bool t

(** Validator from {!type:Repr.t} to [int]. *)
val int : int t

(** Validator from {!type:Repr.t} to [int32]. *)
val int32 : int32 t

(** Validator from {!type:Repr.t} to [int64]. *)
val int64 : int64 t

(** Validator from {!type:Repr.t} to [float]. *)
val float : float t

(** Validator from {!type:Repr.t} to arbitrary numbers (using [float]
    as representation). *)
val number : float t

(** Validator from {!type:Repr.t} to [string]. By default [strict] is
    [true] but when it is [false], it accepts also [bool], [int],
    [float] and a singleton [list] as a valid input. *)
val string : ?strict:bool -> string t

(** Validator from {!type:Repr.t} to [char]. *)
val char : char t

(** Validator from {!type:Repr.t} to [list]. *)
val list : Repr.t list t

(** Validator from {!type:Repr.t} to [non-empty list]. *)
val nel : Repr.t Nel.t t

(** [list_of v] is a validator for list that satisfay [v]. *)
val list_of : 'a t -> 'a list t

(** [nel_of v] is a validator for non-empty-list that satisfay [v]. *)
val nel_of : 'a t -> 'a Nel.t t

(** [option v] is a validator to [option]. *)
val option : 'a t -> 'a option t

(** [sum constrs expr] From the [constrs] list, select the validator
    to use for validating sums. *)
val sum : (string * 'a t) list -> 'a t

(** [result ~ok ~error] is a validator for result. *)
val result : ok:'a t -> error:'b t -> ('a, 'b) result t

(** [either ~left ~right] is a validator for either. *)
val either : left:'a t -> right:'b t -> ('a, 'b) Either.t t

(** [pair f s] is a validator for pair. *)
val pair : 'a t -> 'b t -> ('a * 'b) t

(** [triple f s t] is a validator for triple. *)
val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

val from : (module CHECKABLE with type t = 'a) -> 'a t

(** {2 Predicates} *)

(** [where ?message p] validate using [p] and raise [message] if
    [p x = false]. *)
val where : ?value:Repr.t -> ?message:string -> ('a -> bool) -> ('a, 'a) fn

(** [unless ?message p] validate using [p] and raise [message] if
    [p x = true]. *)
val unless : ?value:Repr.t -> ?message:string -> ('a -> bool) -> ('a, 'a) fn

(** [where_opt ?message p] validate using [p] and raise [message] if
    [p x = None]. *)
val where_opt
  :  ?value:Repr.t
  -> ?message:string
  -> ('a -> 'b option)
  -> ('a, 'b) fn

(** {2 Generic validators}

    Describes a set of generic validators (configured using comparison
    functions) that are specialized in submodules.

    Typically, functions take other functions as arguments to convert
    representations, [to_string] functions, and generic functions such
    as `[equal]`. They can be specialized on demand. *)

(** [equal ?to_repr ?to_string ?eq a] is a validator that ensure that
    the given value is equal (using [eq]) to [a]. If [eq] is not
    provided, it use the [(=)]. *)
val equal
  :  ?to_repr:'a Repr.conv
  -> ?to_string:('a -> string)
  -> ?eq:('a -> 'a -> bool)
  -> 'a
  -> ('a, 'a) fn

(** [not_equal ?to_repr ?to_string ?eq a] is a validator that ensure that
    the given value is not equal (using [eq]) to [a]. If [eq] is not
    provided, it use the [(=)]. *)
val not_equal
  :  ?to_repr:'a Repr.conv
  -> ?to_string:('a -> string)
  -> ?eq:('a -> 'a -> bool)
  -> 'a
  -> ('a, 'a) fn

(** [one_of ?to_repr ?to_string ?eq xs] is a validator that ensure
    that the given value is in the list [xs] (using [eq]). If [eq] is
    not provided, it use the [(=)]. *)
val one_of
  :  ?to_repr:'a Repr.conv
  -> ?to_string:('a -> string)
  -> ?eq:('a -> 'a -> bool)
  -> 'a list
  -> ('a, 'a) fn

(** [gt ?to_repr ?to_string ?cmp a] is a validator that ensure that
    the given value is greater (using [cmp]) than [a]. If [cmp] is not
    provided, it use the [Stdlib.compare]. *)
val gt
  :  ?to_repr:'a Repr.conv
  -> ?to_string:('a -> string)
  -> ?cmp:('a -> 'a -> int)
  -> 'a
  -> ('a, 'a) fn

(** [ge ?to_repr ?to_string ?cmp a] is a validator that ensure that
    the given value is greater or equal  (using [cmp]) than [a]. If [cmp] is not
    provided, it use the [Stdlib.compare]. *)
val ge
  :  ?to_repr:'a Repr.conv
  -> ?to_string:('a -> string)
  -> ?cmp:('a -> 'a -> int)
  -> 'a
  -> ('a, 'a) fn

(** [lt ?to_repr ?to_string ?cmp a] is a validator that ensure that
    the given value is lower (using [cmp]) than [a]. If [cmp] is not
    provided, it use the [Stdlib.compare]. *)
val lt
  :  ?to_repr:'a Repr.conv
  -> ?to_string:('a -> string)
  -> ?cmp:('a -> 'a -> int)
  -> 'a
  -> ('a, 'a) fn

(** [le ?to_repr ?to_string ?cmp a] is a validator that ensure that
    the given value is lower or equal (using [cmp]) than [a]. If [cmp] is not
    provided, it use the [Stdlib.compare]. *)
val le
  :  ?to_repr:'a Repr.conv
  -> ?to_string:('a -> string)
  -> ?cmp:('a -> 'a -> int)
  -> 'a
  -> ('a, 'a) fn

(** [contains ?to_repr ?to_string ?cmp ~min ~max] is a validator that
    ensure that the given value is contains into the the range
    [[min; max]]. *)
val contains
  :  ?to_repr:'a Repr.conv
  -> ?to_string:('a -> string)
  -> ?cmp:('a -> 'a -> int)
  -> min:'a
  -> max:'a
  -> ('a, 'a) fn

(** {1 Dealing with Records} *)

(** [record (fun fields -> ...)] compose validators for records. *)
val record : ((string * Repr.t) list -> 'a record) -> 'a t

(** [opt ?normalize_keys ?alt fields field check] validates the
    optional [field] of a record using [check]. *)
val opt
  :  ?normalize_keys:bool
  -> ?alt:string list
  -> (string * Repr.t) list
  -> string
  -> 'a t
  -> 'a option record

(** [req ?normalize_keys ?alt fields field check] validates the
    [required field] of a record using [check]. *)
val req
  :  ?normalize_keys:bool
  -> ?alt:string list
  -> (string * Repr.t) list
  -> string
  -> 'a t
  -> 'a record

(** [guard ?normalize_keys ?alt fields field check] perform [check] as
    a conditon. *)
val guard
  :  ?normalize_keys:bool
  -> ?alt:string list
  -> (string * Repr.t) list
  -> string
  -> 'a t
  -> unit record

(** [use_record fields record_check] allows you to use a record
    validator in a validation pipeline for another record (enables
    reusability in record validators). *)
val use_record : (string * Repr.t) list -> 'a t -> 'a record

(** {1 Error propagation} *)

(** [fail_with ?repr message] fails validation with a given
    [message]. *)
val fail_with : ?value:Repr.t -> string -> ('a, value_error) result

(** {1 Infix operators} *)

module Infix : sig
  (** Some infix helpers. *)

  (** [f <$> x] is [Result.map f x]. *)
  val ( <$> ) : ('a -> 'b) -> ('a, 'c) result -> ('b, 'c) result

  (** [fm $ f] compose a function that return a result with a regular
      function.*)
  val ( $ ) : ('a -> ('b, 'c) result) -> ('b -> 'd) -> 'a -> ('d, 'c) result

  (** [f & g] is a kleisli composition function, it compose two
      function that returns result. *)
  val ( & )
    :  ('a -> ('b, 'c) result)
    -> ('b -> ('d, 'c) result)
    -> 'a
    -> ('d, 'c) result

  (** [(f / g) x] is the choice operator (or alt). Performs [g x] if
      [f x] does not succeed. *)
  val ( / )
    :  ('a -> ('b, 'c) result)
    -> ('a -> ('b, 'd) result)
    -> 'a
    -> ('b, 'd) result
end

include module type of Infix

(** {1 Binding operators} *)

module Syntax : sig
  (** Some bindings helpers. *)

  (** [map] binding operator. *)
  val ( let+ ) : ('a, 'b) result -> ('a -> 'c) -> ('c, 'b) result

  (** [bind] binding operator. *)
  val ( let* ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result

  (** Monoidal product (errors are a Semigroup, a Nel, so it collapse errors).
  *)
  val ( and+ )
    :  ('a, 'b Nel.t) result
    -> ('c, 'b Nel.t) result
    -> ('a * 'c, 'b Nel.t) result

  (** Same as [and+]. *)
  val ( and* )
    :  ('a, 'b Nel.t) result
    -> ('c, 'b Nel.t) result
    -> ('a * 'c, 'b Nel.t) result
end

include module type of Syntax

(** {1 Validators for OCaml types}

    A set of validators for well-known OCaml types. *)

module Int : sig
  (** Validators for integers. *)

  (** [is_positive] ensure that the given int is positive. *)
  val is_positive : (int, int) fn

  (** [is_negative] ensure that the given int is negative. *)
  val is_negative : (int, int) fn

  (** [is_odd] ensure that the given int is odd. *)
  val is_odd : (int, int) fn

  (** [is_even] ensure that the given int is even. *)
  val is_even : (int, int) fn

  (** {!val:equal} for [int]. *)
  val equal : int -> (int, int) fn

  (** {!val:not_equal} for [int]. *)
  val not_equal : int -> (int, int) fn

  (** {!val:one_of} for [int]. *)
  val one_of : int list -> (int, int) fn

  (** {!val:gt} for [int]. *)
  val gt : int -> (int, int) fn

  (** {!val:ge} for [int]. *)
  val ge : int -> (int, int) fn

  (** {!val:lt} for [int]. *)
  val lt : int -> (int, int) fn

  (** {!val:le} for [int]. *)
  val le : int -> (int, int) fn

  (** {!val:contains} for [int]. *)
  val contains : min:int -> max:int -> (int, int) fn

  (** {!val:where} for [int]. *)
  val where : ?message:string -> (int -> bool) -> (int, int) fn

  (** {!val:where_opt} for [int]. *)
  val where_opt : ?message:string -> (int -> 'b option) -> (int, 'b) fn
end

module Int32 : sig
  (** Validators for int32. *)

  (** [is_positive] ensure that the given int is positive. *)
  val is_positive : (int32, int32) fn

  (** [is_negative] ensure that the given int is negative. *)
  val is_negative : (int32, int32) fn

  (** [is_odd] ensure that the given int is odd. *)
  val is_odd : (int32, int32) fn

  (** [is_even] ensure that the given int is even. *)
  val is_even : (int32, int32) fn

  (** {!val:equal} for [int32]. *)
  val equal : int32 -> (int32, int32) fn

  (** {!val:not_equal} for [int32]. *)
  val not_equal : int32 -> (int32, int32) fn

  (** {!val:one_of} for [int32]. *)
  val one_of : int32 list -> (int32, int32) fn

  (** {!val:gt} for [int32]. *)
  val gt : int32 -> (int32, int32) fn

  (** {!val:ge} for [int32]. *)
  val ge : int32 -> (int32, int32) fn

  (** {!val:lt} for [int32]. *)
  val lt : int32 -> (int32, int32) fn

  (** {!val:le} for [int32]. *)
  val le : int32 -> (int32, int32) fn

  (** {!val:contains} for [int32]. *)
  val contains : min:int32 -> max:int32 -> (int32, int32) fn

  (** {!val:where} for [int32]. *)
  val where : ?message:string -> (int32 -> bool) -> (int32, int32) fn

  (** {!val:where_opt} for [int32]. *)
  val where_opt : ?message:string -> (int32 -> 'b option) -> (int32, 'b) fn
end

module Int64 : sig
  (** Validators for int64. *)

  (** [is_positive] ensure that the given int is positive. *)
  val is_positive : (int64, int64) fn

  (** [is_negative] ensure that the given int is negative. *)
  val is_negative : (int64, int64) fn

  (** [is_odd] ensure that the given int is odd. *)
  val is_odd : (int64, int64) fn

  (** [is_even] ensure that the given int is even. *)
  val is_even : (int64, int64) fn

  (** {!val:equal} for [int64]. *)
  val equal : int64 -> (int64, int64) fn

  (** {!val:not_equal} for [int64]. *)
  val not_equal : int64 -> (int64, int64) fn

  (** {!val:one_of} for [int64]. *)
  val one_of : int64 list -> (int64, int64) fn

  (** {!val:gt} for [int64]. *)
  val gt : int64 -> (int64, int64) fn

  (** {!val:ge} for [int64]. *)
  val ge : int64 -> (int64, int64) fn

  (** {!val:lt} for [int64]. *)
  val lt : int64 -> (int64, int64) fn

  (** {!val:le} for [int64]. *)
  val le : int64 -> (int64, int64) fn

  (** {!val:contains} for [int64]. *)
  val contains : min:int64 -> max:int64 -> (int64, int64) fn

  (** {!val:where} for [int64]. *)
  val where : ?message:string -> (int64 -> bool) -> (int64, int64) fn

  (** {!val:where_opt} for [int64]. *)
  val where_opt : ?message:string -> (int64 -> 'b option) -> (int64, 'b) fn
end

module Float : sig
  (** Validators for floats. *)

  (** [is_positive] ensure that the given float is positive. *)
  val is_positive : (float, float) fn

  (** [is_negative] ensure that the given float is negative. *)
  val is_negative : (float, float) fn

  (** [is_odd] ensure that the given float is odd. *)
  val is_odd : (float, float) fn

  (** [is_even] ensure that the given float is even. *)
  val is_even : (float, float) fn

  (** {!val:equal} for [float]. *)
  val equal : float -> (float, float) fn

  (** {!val:not_equal} for [float]. *)
  val not_equal : float -> (float, float) fn

  (** {!val:one_of} for [float]. *)
  val one_of : float list -> (float, float) fn

  (** {!val:gt} for [float]. *)
  val gt : float -> (float, float) fn

  (** {!val:ge} for [float]. *)
  val ge : float -> (float, float) fn

  (** {!val:lt} for [float]. *)
  val lt : float -> (float, float) fn

  (** {!val:le} for [float]. *)
  val le : float -> (float, float) fn

  (** {!val:contains} for [float]. *)
  val contains : min:float -> max:float -> (float, float) fn

  (** {!val:where} for [float]. *)
  val where : ?message:string -> (float -> bool) -> (float, float) fn

  (** {!val:where_opt} for [float]. *)
  val where_opt : ?message:string -> (float -> 'b option) -> (float, 'b) fn
end

module String : sig
  (** Validators for strings. *)

  (** {!val:equal} for [string]. *)
  val equal : string -> (string, string) fn

  (** {!val:not_equal} for [string]. *)
  val not_equal : string -> (string, string) fn

  (** {!val:one_of} for [string]. *)
  val one_of : string list -> (string, string) fn

  (** {!val:where} for [string]. *)
  val where : ?message:string -> (string -> bool) -> (string, string) fn

  (** {!val:where_opt} for [string]. *)
  val where_opt : ?message:string -> (string -> 'b option) -> (string, 'b) fn

  (** A validator that ensure that the given string is not empty. *)
  val not_empty : (string, string) fn

  (** A validator that ensure that the given string is not blank. *)
  val not_blank : (string, string) fn

  (** [has_length n] ensure that the given string has the length
      [n]. *)
  val has_length : int -> (string, string) fn

  (** [length_between ~min ~max] ensure that the length of the given
      string is [>= min] and [<= max]. *)
  val length_between : min:int -> max:int -> (string, string) fn

  (** [minimal_length l] ensure that the length of the given string is
      [>= l]. *)
  val minimal_length : int -> (string, string) fn

  (** [maximal_length l] ensure that the length of the given string is
      [<= l]. *)
  val maximal_length : int -> (string, string) fn

  (** [has_prefix prefix] ensure that the given string starts with the
      given [prefix]. *)
  val has_prefix : string -> (string, string) fn

  (** [has_suffix suffix] ensure that the given string ends with the
      given [suffix]. *)
  val has_suffix : string -> (string, string) fn
end

module Char : sig
  (** Validators for chars. *)

  (** {!val:equal} for [char]. *)
  val equal : char -> (char, char) fn

  (** {!val:not_equal} for [char]. *)
  val not_equal : char -> (char, char) fn

  (** {!val:one_of} for [char]. *)
  val one_of : char list -> (char, char) fn

  (** {!val:where} for [char]. *)
  val where : ?message:string -> (char -> bool) -> (char, char) fn

  (** {!val:where_opt} for [char]. *)
  val where_opt : ?message:string -> (char -> 'b option) -> (char, 'b) fn

  (** Ensure that the given char is a digit ([0..9]). *)
  val is_digit : (char, char) fn

  (** Ensure that the given char is a digit ([0..9]) and convert-it. *)
  val as_digit : (char, int) fn

  (** Ensure that the given char is an hexadecimal digit
      ([0..9 | a..f | A..F]). *)
  val is_hex_digit : (char, char) fn

  (** Ensure that the given char is an hexadecimal digit
      ([0..9 | a..f | A..F]) and convert-it. *)
  val as_hex_digit : (char, int) fn

  (** Ensure that the given char is an alpha char
      ([a..z | A..Z]). *)
  val is_alpha : (char, char) fn

  (** Ensure that the given char is an alphanumeric char
      ([0..9 | a..z | A..Z]). *)
  val is_alphanumeric : (char, char) fn

  (** Ensure that the given char is a lowercase char ([a..z]). *)
  val is_lowercase : (char, char) fn

  (** Ensure that the given char is an uppercase char ([A..Z]). *)
  val is_uppercase : (char, char) fn

  (** Ensure that the given char is a whitespace char. *)
  val is_whitespace : (char, char) fn

  (** Ensure that the given char is a newline char. *)
  val is_newline : (char, char) fn
end
