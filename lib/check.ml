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

let invalid_field ?(alt = []) field error =
  Error.Invalid_field { field = Nel.(make field alt); error }
;;

let rec check r k =
  match r, k with
  | Repr.Null, Kind.Null -> Ok ()
  | Repr.Bool _, Kind.Bool -> Ok ()
  | Repr.Int _, Kind.Int -> Ok ()
  | Repr.Float _, Kind.Float -> Ok ()
  | Repr.List lr, Kind.List k ->
    let _i, mapped_result =
      List.fold_left
        (fun (i, acc) value ->
           let acc =
             match acc, check value k with
             | Ok xs, Ok x -> Ok (x :: xs)
             | Error xs, Error x -> Error (Nel.cons (i, x) xs)
             | Error e, _ -> Error e
             | _, Error e -> Error (Nel.singleton (i, e))
           in
           i + 1, acc)
        (0, Ok [])
        lr
    in
    mapped_result
    |> Result.map (fun _ -> ())
    |> Result.map_error (fun errors -> Error.invalid_list r (Nel.rev errors))
  | Repr.Record lr, Kind.Record lk ->
    let mapped_result =
      List.fold_left
        (fun acc (name, value) ->
           let kind = List.find_opt (fun (n, _) -> n = name) lk in
           let _, kind = Option.value kind ~default:(name, Kind.null) in
           (* TODO manage fields in type but not in term *)
           match acc, check value kind with
           | Ok xs, Ok x -> Ok (x :: xs)
           | Error xs, Error x -> Error (Nel.cons (invalid_field name x) xs)
           | Error e, _ -> Error e
           | _, Error e -> Error (Nel.singleton (invalid_field name e)))
        (Ok [])
        lr
    in
    mapped_result
    |> Result.map (fun _ -> ())
    |> Result.map_error (fun errors -> Error.invalid_record r (Nel.rev errors))
  | _, Kind.Any -> Ok ()
  | _, Kind.Or lk -> check_or r k lk
  | _ -> Error (Error.unexpected_kind k r)

and check_or r k = function
  | [] -> Error (Error.unexpected_kind k r) (* this result is dropped *)
  | k :: lk ->
    (match check r k with
     | Error _ -> check_or r k lk
     | result -> result)
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
         value
         |> v
         |> Result.map Option.some
         |> Result.map_error (Error.invalid_field ~alt key))
  in
  aux (key :: alt)
;;

let req ?(normalize_keys = true) ?(alt = []) fields key v =
  let rec aux = function
    | [] -> Error (Error.missing_field ~alt key)
    | x :: xs ->
      (match Misc.find_assoc ~normalize_keys fields x with
       | None -> aux xs
       | Some Repr.Null ->
         (* HACK: We want to handle optional field inside requirement
            validation. *)
         Repr.Null
         |> v
         |> Result.map_error (fun _ -> Error.missing_field ~alt key)
       | Some value ->
         value |> v |> Result.map_error (Error.invalid_field ~alt key))
  in
  aux (key :: alt)
;;

let use_record fields v =
  Repr.record fields |> v |> Result.map_error Error.invalid_subrecord
;;
