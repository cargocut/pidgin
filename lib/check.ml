(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

type 'a value = ('a, Error.for_value) result
type ('a, 'b) fn = 'a -> 'b value
type 'a t = (Repr.t, 'a) fn
type 'a record = ('a, Error.for_record Nel.t) result

module Infix = struct
  let ( <$> ) = Result.map
  let ( $ ) l f x = Result.map f (l x)
  let ( & ) l r x = Result.bind (l x) r
  let ( / ) l r x = Result.fold ~ok:Result.ok ~error:(fun _ -> r x) (l x)
end

module Syntax = struct
  let ( let+ ) x f = Result.map f x
  let ( let* ) = Result.bind

  let ( and+ ) a b =
    match a, b with
    | Ok x, Ok y -> Ok (x, y)
    | Error a, Error b -> Error (Nel.append a b)
    | Error err, _ | _, Error err -> Error err
  ;;

  let ( and* ) = ( and+ )
end

include Infix
include Syntax

let unexpected_kind kind expr =
  expr |> Error.unexpected_kind kind |> Result.error
;;

let null = function
  | Repr.Null -> Ok ()
  | x -> unexpected_kind Kind.null x
;;

let bool = function
  | Repr.Bool b -> Ok b
  | x -> unexpected_kind Kind.bool x
;;

let int = function
  | Repr.Int i -> Ok i
  | x -> unexpected_kind Kind.int x
;;

let float = function
  | Repr.Float f -> Ok f
  | Repr.Int i ->
    (* KLUDGE: Ugly trick because of the infamouse [number] type in
       JavaScript *)
    Ok (float_of_int i)
  | x -> unexpected_kind Kind.float x
;;

let string = function
  | Repr.String s -> Ok s
  | x -> unexpected_kind Kind.string x
;;

let list = function
  | Repr.List xs -> Ok xs
  | x ->
    (* NOTE: Since it can handle every repr, we probably do not want
       to build a complicated kind here. *)
    unexpected_kind Kind.(list any) x
;;

let list_of v = function
  | Repr.List xs as value ->
    let _i, mapped_result =
      List.fold_left
        (fun (i, acc) value ->
           let acc =
             match acc, v value with
             | Ok xs, Ok x -> Ok (x :: xs)
             | Error xs, Error x -> Error (Nel.cons (i, x) xs)
             | Error e, _ -> Error e
             | _, Error e -> Error (Nel.singleton (i, e))
           in
           i + 1, acc)
        (0, Ok [])
        xs
    in
    mapped_result
    |> Result.map List.rev
    |> Result.map_error (fun errors ->
      Error.invalid_list value (Nel.rev errors))
  | x ->
    (* NOTE: we cannot inspect the validator [v] here, so we lose the
       kind information. *)
    unexpected_kind Kind.(list any) x
;;

let option some = function
  | Repr.Null -> Ok None
  | value -> Option.some <$> some value
;;

let k_sum_or_any constrs =
  match constrs with
  | [] ->
    (* KLUDGE: We can relay on non-empty list but it looks heavy.
       There is no [Kind.absurd] because it is an internal
       representation. *)
    Kind.record [ "absurd", Kind.any ]
  | x :: xs ->
    (* KLUDGE: since kind are not deductible from validators, we
       lose that information. *)
    Kind.sum Nel.(map (fun (c, _) -> c, Kind.any) (x :: xs))
;;

let record v = function
  | Repr.Record fields as value ->
    fields |> v |> Result.map_error (Error.invalid_record value)
  | x ->
    (* NOTE: we cannot inspect the validator [v] here, so we lose the
       kind information for record classification. *)
    unexpected_kind Kind.(record []) x
;;

let opt ?(normalize_keys = true) ?(alt = []) fields key v =
  let rec aux = function
    | [] -> Ok None
    | x :: xs ->
      (match Misc.find_assoc ~normalize_keys fields x with
       | None -> aux xs
       | Some Repr.Null -> Ok None
       | Some value ->
         (* NOTE: If the field exists we perform the validation. We
            don't skip the result if the validation is invalid
            because... it's optional, sure, but not lax!*)
         value
         |> v
         |> Result.map Option.some
         |> Result.map_error (Error.invalid_field ~alt key))
  in
  aux (key :: alt)
;;

let handle_null ~alt key v =
  (* HACK: We want to handle optional field inside requirement
     validation. *)
  Repr.Null |> v |> Result.map_error (fun _ -> Error.missing_field ~alt key)
;;

let req ?(normalize_keys = true) ?(alt = []) fields key v =
  let rec aux = function
    | [] -> handle_null ~alt key v
    | x :: xs ->
      (match Misc.find_assoc ~normalize_keys fields x with
       | None -> aux xs
       | Some Repr.Null -> handle_null ~alt key v
       | Some value ->
         value |> v |> Result.map_error (Error.invalid_field ~alt key))
  in
  aux (key :: alt)
;;

let use_record fields v =
  Repr.record fields |> v |> Result.map_error Error.invalid_subrecord
;;

let rec sum constrs = function
  (* Deal with real records *)
  | ( Repr.Record [ ("constr", String constr); ("value", value) ]
    | Repr.Record [ ("value", value); ("constr", String constr) ] ) as repr ->
    constr
    |> Misc.find_assoc constrs
    |> Option.fold
         ~none:(unexpected_kind (k_sum_or_any constrs) repr)
         ~some:(fun v -> v value)
  (* Deal with desugaring *)
  | Repr.String constr
  | Repr.List [ String constr ]
  | Repr.List [ String constr; Null ]
  | Repr.Record [ ("constr", String constr) ] ->
    sum constrs ((Repr.sum (fun () -> constr, Repr.null ())) ())
  | Repr.List [ String constr; v ] ->
    sum constrs ((Repr.sum (fun () -> constr, v)) ())
  | repr ->
    (* Error handling *)
    unexpected_kind (k_sum_or_any constrs) repr
;;

let result ~ok ~error =
  sum [ "ok", ok $ Result.ok; "error", error $ Result.error ]
;;

let either ~left ~right =
  sum [ "left", left $ Either.left; "right", right $ Either.right ]
;;

let rec pair fst snd = function
  | Repr.Record [ _; _ ] as repr ->
    record
      (fun fields ->
         let+ a = req fields "first" ~alt:[ "fst" ] fst
         and+ b = req fields "second" ~alt:[ "snd" ] snd in
         a, b)
      repr
  | List [ a; b ] -> pair fst snd (Repr.pair Fun.id Fun.id (a, b))
  | List [ a ] -> pair fst snd (Repr.pair Fun.id Repr.null (a, ()))
  | List [] ->
    pair fst snd Repr.(record [ "first", null (); "second", null () ])
  | repr -> unexpected_kind Kind.(pair any any) repr
;;

let rec triple f s t = function
  | Repr.List [ a; b; c ] ->
    triple f s t (Repr.triple Fun.id Fun.id Fun.id (a, b, c))
  | Repr.List [ a; b ] ->
    triple f s t Repr.(triple Fun.id Fun.id null (a, b, ()))
  | Repr.List [ a ] -> triple f s t Repr.(triple Fun.id null null (a, (), ()))
  | repr -> (pair f (pair s t) $ fun (a, (b, c)) -> a, b, c) repr
;;
