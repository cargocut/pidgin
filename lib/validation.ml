(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

(* Temporary *)
type value_error =
  | Invalid_shape of
      { expected : string
      ; given : Data.t
      }
  | Invalid_list of
      { errors : (int * value_error) list
      ; given : Data.t list
      }

type 'a t = ('a, value_error) result

let invalid_shape expected given = Error (Invalid_shape { expected; given })

let null =
  let open Data.Deconstruct in
  let error = invalid_shape "null" in
  fun t -> fold_opt ~null:(fun _ -> Ok ()) ~default:error t
;;

let bool =
  let open Data.Deconstruct in
  let error = invalid_shape "bool" in
  fun t -> fold_opt ~bool:(fun b -> Ok b) ~default:error t
;;

let int =
  let open Data.Deconstruct in
  let error = invalid_shape "int" in
  fun t -> fold_opt ~int:(fun i -> Ok i) ~default:error t
;;

let float =
  let open Data.Deconstruct in
  let error = invalid_shape "float" in
  fun t -> fold_opt ~float:(fun f -> Ok f) ~default:error t
;;

let string =
  let open Data.Deconstruct in
  let error = invalid_shape "string" in
  fun ?(strict = true) t ->
    fold_opt
      ~string:(fun s -> Ok s)
      ~default:(fun t ->
        if strict
        then error t
        else
          fold_opt
            ~bool:(fun b -> Ok (string_of_bool b))
            ~int:(fun i -> Ok (string_of_int i))
            ~float:(fun f -> Ok (string_of_float f))
            ~default:error
            t)
      t
;;

let list_of v =
  let open Data.Deconstruct in
  let error = invalid_shape "list" in
  let merge i acc value =
    match value with
    | Ok a -> Result.map (fun ok -> a :: ok) acc
    | Error b ->
      Result.map_error
        (fun error -> (i, b) :: error)
        (Result.bind acc (fun _ -> Error []))
  in
  fun t ->
    fold_opt
      ~list:(fun l ->
        List.fold_left
          (fun (i, acc) x -> succ i, merge i acc @@ v x)
          (0, Ok [])
          l
        |> snd
        |> Result.map List.rev
        |> Result.map_error (fun errors -> Invalid_list { errors; given = l }))
      ~default:error
      t
;;

let record_of _v _t = failwith "Not yet implemented"
