(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

(** Describes a validation tree for validating complex structured data
    from {!type:Repr.t}. *)

(** {1 Types} *)

(** A type describing a value that has been validated. *)
type 'a value = ('a, Error.for_value) result

(** A type describing a record that has been validated. *)
type 'a record = ('a, Error.for_record Nel.t) result

(** A type describing a function that perform validation (a check). *)
type ('a, 'b) fn = 'a -> 'b value

(** A type describing a function that perform validation (a check)
    from a {!type:Repr.t}. *)
type 'a t = (Repr.t, 'a) fn

(** {1 Data Validation} *)

(** Validator from {!type:Repr.t} to [unit]. *)
val null : unit t

(** Validator from {!type:Repr.t} to [bool]. *)
val bool : bool t

(** Validator from {!type:Repr.t} to [int]. *)
val int : int t

(** Validator from {!type:Repr.t} to [float]. *)
val float : float t

(** Validator from {!type:Repr.t} to [string]. *)
val string : string t

(** Validator from {!type:Repr.t} to [list]. *)
val list : Repr.t list t

(** [list_of v] is a validator for list that satisfay [v]. *)
val list_of : 'a t -> 'a list t

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

(** [use_record fields record_check] allows you to use a record
    validator in a validation pipeline for another record (enables
    reusability in record validators). *)
val use_record : (string * Repr.t) list -> 'a t -> 'a record

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
